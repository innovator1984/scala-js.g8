package app.swagger

import app.SwaggerCodegen.property2Scala
import better.files.File
import io.swagger.models.Swagger
import utils.CaseClassMetaHelper
import utils.ScalaFmtHelper
import scala.meta._
import scala.meta.Source
import scala.meta.Stat
import better.files._
import scala.collection.JavaConversions._
import app._

object ModelsGen {
  def gen(swagger: Swagger, apiVersion: String, f: File): Unit = {
    println(s"- Starting Models Generator for $dollar${f.pathAsString}")
    val modelsFolder = file"shared/src/main/scala/shared/models/swagger/$dollar${f.nameWithoutExtension}/$dollar$apiVersion"

    swagger.getDefinitions.toVector.foreach {
      case (name, model) =>
        val modelName = name.toUpperCamelCase

        val propertiesAsScala: Vector[String] = model.getProperties.toVector.map { e =>
          s"$dollar${e._1.toCamelCase}: $dollar${property2Scala(e._2)}"
        }

        val modelAsCaseClass = s"case class $dollar$modelName($dollar${propertiesAsScala.mkString(", ")})"

        val targetFile = modelsFolder./(s"$dollar$modelName.scala")
        if (targetFile.notExists) {
          // Create Template
          val template =
            s"""
               |package shared.models.swagger.$dollar${f.nameWithoutExtension}.$dollar$apiVersion
               |
            |import java.time._
               |
            |$dollar$modelAsCaseClass
          """.trim.stripMargin

          targetFile.createIfNotExists(createParents = true).overwrite(template)
        } else {
          // Update existing Source
          val source = targetFile.toJava.parse[Source].get
          val caseClassStat =
            modelAsCaseClass.parse[Stat].get
          val tree = CaseClassMetaHelper.updateOrInsert(source, caseClassStat)
          targetFile.write(ScalaFmtHelper.formatCode(tree.syntax))
        }
    }
  }

}
