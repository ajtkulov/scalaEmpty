import scala.annotation.StaticAnnotation
import scala.meta._

class noCompanion extends StaticAnnotation

class Companion extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    println(defn.getClass)
    println(defn)
    defn match {
      case q"${cls: Defn.Class}; ${companion: Defn.Object}" => println("****");(cls, companion)
//      case cls: Defn.Object => println("object")
      case cls: Defn.Class => println("class")
//      case cls => println("qwe"); //abort("wer")
      case cls => println("other")
    }

    defn
  }
}
