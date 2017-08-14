import scala.annotation.StaticAnnotation
import scala.meta._

class noCompanion extends StaticAnnotation

class Companion extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case cls: Defn.Object => println("object")
      case cls: Defn.Class => println("class")
      case cls => println("qwe"); //abort("wer")
    }

    defn
  }
}
