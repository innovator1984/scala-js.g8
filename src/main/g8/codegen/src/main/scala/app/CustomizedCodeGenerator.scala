// scalafmt: { maxColumn = 120 }
package app

import slick.codegen.SourceCodeGenerator
import slick.sql.SqlProfile.ColumnOption
import slick.{model => m}

class CustomizedCodeGenerator(val model: m.Model) extends SourceCodeGenerator(model) {
  override val ddlEnabled = false

  override def entityName: (String) => String =
    (dbName: String) => dbName.split("_").map(_.capitalize).mkString

  override def tableName: (String) => String = (dbName: String) => entityName(dbName) + "Table"

  override def Table = new Table(_) { table =>

    val E: String = entityName(model.name.table)
    val T: String = tableName(model.name.table)
    val Q: String = TableValue.rawName

    override def TableValue = new TableValue {
      override def rawName: String = {
        val raw = entityName(model.name.asString).uncapitalize
        if (raw.endsWith("s")) raw else raw + "s"
      }
    }

    override def autoIncLastAsOption = true

    override def EntityType = new EntityTypeDef {
      override def doc: String = ""

      override def code: String = {
        val args = columns
          .map(
            c =>
              c.default
                .map(v => s"$dollar${c.name}: $dollar${c.exposedType} = $dollar$v")
                .getOrElse(
                  s"$dollar${c.name}: $dollar${c.exposedType}"
              ))
          .mkString(", ")

        val prns = parents.map(" with " + _).mkString("")

        s"""
           |case class $dollar$name($dollar$args) $dollar$prns
             """.stripMargin
      }
    }

    override def Column = new Column(_) { column =>
      override def rawType: String = model.tpe match {
        case "java.sql.Date" => "LocalDate"
        case "java.sql.Time" => "LocalTime"
        case "java.sql.Timestamp" =>
          model.options
            .find(_.isInstanceOf[ColumnOption.SqlType])
            .map(_.asInstanceOf[ColumnOption.SqlType].typeName)
            .map {
              case "timestamptz" => "OffsetDateTime"
              case _             => "LocalDateTime"
            } getOrElse "LocalDateTime"
        case "String" =>
          model.options
            .find(_.isInstanceOf[ColumnOption.SqlType])
            .map(_.asInstanceOf[ColumnOption.SqlType].typeName)
            .map {
              case "json" | "jsonb" => "Json"
              case "hstore"         => "Map[String, String]"
              case "_text"          => "List[String]"
              case "_varchar"       => "List[String]"
              case "geometry"       => "com.vividsolutions.jts.geom.Geometry"
              case "int8[]"         => "List[Long]"
              case "interval"       => "Duration"
              case e                => "String"
            } getOrElse "String"
        case _ => super.rawType.asInstanceOf[String]
      }

      override def code =
        s"""val $dollar$name: Rep[$dollar$actualType] = column[$dollar$actualType]("$dollar${model.name}"$dollar${options
          .filter(_ => !rawType.startsWith("List"))
          .map(", " + _)
          .mkString("")})"""

      override def rawName: String = entityName(model.name).uncapitalize
    }
  }

  override def packageCode(profile: String, pkg: String, container: String, parentType: Option[String]): String = {
    s"""
       |package $dollar$pkg
       |
       |import bay.driver.CustomizedPgDriver
       |import java.time._
       |import io.circe._
       |import shared.models.slick.$dollar${ExtString(container).toCamelCase}._
       |
       |object $dollar$container extends {
       |  val profile = bay.driver.CustomizedPgDriver
       |} with $dollar$container
       |
       |trait $dollar$container$dollar${parentType.map(t => s" extends $dollar$t").getOrElse("")} {
       |
       |  val profile: bay.driver.CustomizedPgDriver
       |  import profile.api._
       |
       |  $dollar${indent(code.replace("$dollar$CONTAINER", container))}
       |
       |}
     """.stripMargin
  }
}
