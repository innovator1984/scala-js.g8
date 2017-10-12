// scalafmt: { maxColumn = 120 }
package app

import java.io.{File => JFile}
import java.sql.DriverManager

import bay.driver.CustomizedPgDriver
import better.files._
import com.typesafe.config.ConfigFactory
import org.flywaydb.core.Flyway
import utils.CaseClassMetaHelper
import utils.ScalaFmtHelper

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.meta._

object DbCodegen extends App {
  import scala.collection.JavaConversions._
  import scala.concurrent.ExecutionContext.Implicits.global

  val configFile = file"server/conf/application.conf"
  println("Using Configuration File: " + configFile.pathAsString)
  val config = ConfigFactory.parseFile(configFile.toJava).resolve()

  println("# Starting databse codegeneration")
  for {
    dbConfig <- config.getObject("slick.dbs")
  } yield {
    val short = dbConfig._1
    val name = short.capitalize

    val url = config.getString(s"slick.dbs.$dollar$short.db.url")
    val driver = config.getString(s"slick.dbs.$dollar$short.db.driver")
    val user = config.getString(s"slick.dbs.$dollar$short.db.user")
    val password = config.getString(s"slick.dbs.$dollar$short.db.password")

    val excluded = List("schema_version") ++ config.getStringList(s"slick.dbs.$dollar$short.db.exclude")

    val profile = CustomizedPgDriver
    val db = CustomizedPgDriver.api.Database.forURL(url, driver = driver, user = user, password = password)

    if (args.contains("recreate")) {
      println("- Removing Database to rerun all migrations")
      val c = DriverManager.getConnection(url.reverse.dropWhile(_ != '/').reverse, user, password)
      val statement = c.createStatement()
      try {
        statement.executeUpdate(s"DROP DATABASE $dollar${url.reverse.takeWhile(_ != '/').reverse};")
      } catch {
        case scala.util.control.NonFatal(e) => ()
      } finally {
        statement.close()
        c.close()
      }
    }

    println("- Creating Database if necessary")
    val c = DriverManager.getConnection(url.reverse.dropWhile(_ != '/').reverse, user, password)
    val statement = c.createStatement()
    try {
      statement.executeUpdate(s"CREATE DATABASE $dollar${url.reverse.takeWhile(_ != '/').reverse};")
    } catch {
      case scala.util.control.NonFatal(e) =>
    } finally {
      statement.close()
      c.close()
    }

    println("- Migrating using flyway..")
    val flyway = new Flyway
    flyway.setDataSource(url, user, password)
    flyway.setValidateOnMigrate(false) // Creates problems with windows machines
    flyway.setLocations(s"filesystem:server/conf/db/migration/$dollar$short")
    flyway.migrate()

    println("- Starting codegeneration task..")
    def sourceGen =
      db.run(profile.createModel(Option(profile.defaultTables.map(ts =>
          ts.filterNot(t => excluded contains t.name.name)))))
        .map { model =>
          new CustomizedCodeGenerator(model)
        }

    Await.ready(
      sourceGen
        .map(
          codegen =>
            codegen.writeToFile("bay.driver.CustomizedPgDriver",
                                "dbschema/src/main/scala",
                                "models.slick",
                                name,
                                s"$dollar$name.scala"))
        .recover {
          case e: Throwable => e.printStackTrace()
        },
      Duration.Inf
    )

    println("- Parsing generated slick-model")
    val createdFile = file"dbschema/src/main/scala/models/slick/$dollar$name.scala"
    val modelSource = createdFile.contentAsString
    val sharedCaseClasses =
      modelSource.split("\n").map(_.trim).filter(_.startsWith("case class"))
    val filteredSource = modelSource
      .split("\n")
      .filterNot(_.trim.startsWith("case class"))
      .mkString("\n")
    println("Saving filtered slick-model")
    createdFile.overwrite(filteredSource)

    // Creating Shared Models
    val path =
      file"shared/src/main/scala/shared/models/slick/$dollar${name.toCamelCase}"
    sharedCaseClasses.foreach { caseClass =>
      val caseClassStat = caseClass.parse[Stat].get
      val modelName = caseClassStat.collect {
        case q"case class $dollar$tname (...$dollar$paramss)" =>
          tname.value
      }.head

      val targetFile = path./(s"$dollar$modelName.scala")

      if (targetFile.notExists) {
        println(s"-- Creating $dollar${targetFile.path.toString}")

        val template =
          s"""
               |package shared.models.slick.$dollar${name.toCamelCase}
               |
               |import shared.utils.Codecs._
               |import java.time._
               |
               |$dollar$caseClass
          """.trim.stripMargin

        targetFile.createIfNotExists(createParents = true).overwrite(template)
      } else {
        println(s"-- Loading $dollar${targetFile.path.toString}")

        val source = targetFile.contentAsString.parse[Source].get
        val tree = CaseClassMetaHelper.updateOrInsert(source, caseClassStat)
        targetFile.write(ScalaFmtHelper.formatCode(tree.syntax))
      }
    }
  }
}
