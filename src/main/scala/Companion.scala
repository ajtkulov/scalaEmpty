import scala.annotation.StaticAnnotation
import scala.meta._

class NoCompanion extends StaticAnnotation

class Companion extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    println(defn)
    defn match {
//      case cls: Defn.Object => println("qwe")
//      case cls: Defn.Class => println("qwe")
      case cls => println("qwe"); abort("wer")
    }

    defn
  }
}
