import org.scalatest.FunSuite
import org.scalatest._, Matchers._
class Test1 extends FunSuite {
  test("test-1") {


    @Companion
    case class Some1(a: Int)

    """1 + 1""" should compile


  }

}
