package utils

import scala.meta._

object CaseClassMetaHelper {
  def updateOrInsert(source: Source, caseClass: Stat): Tree = {
    val modelName = caseClass.collect {
      case q"case class $dollar$tname (...$dollar$paramss)" =>
        tname.value
    }.head

    source.transform {
      case c @ q"case class $dollar$tname (...$dollar$paramss)" if tname.value == modelName =>
        println(s"- Updating CaseClass $dollar$modelName")
        caseClass
      case c @ q"case class $dollar$tname (...$dollar$paramss) { ..$dollar$body }" if tname.value == modelName =>
        println(s"- Updating CaseClass $dollar$modelName (preserving body)")
        caseClass.transform {
          case q"case class $dollar$tname (...$dollar$paramss)" =>
            q"case class $dollar$tname (...$dollar$paramss) { ..$dollar$body }"
        }
      case q"trait $dollar$tname { ..$dollar$body }" if source.collect {
        case q"case class $dollar$cname (...$dollar$paramss)" if cname.value == modelName             => 1
        case q"case class $dollar$cname (...$dollar$paramss) { ..$dollar$body }" if cname.value == modelName => 1
      }.isEmpty =>
        println(s"- Can't find $dollar$modelName, adding it to trait $dollar$tname")
        q"trait $dollar$tname { ..$dollar${body :+ caseClass} }"
    }
  }
}
