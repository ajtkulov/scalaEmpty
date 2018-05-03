import model.{Coor, Geometry, Poly}
import org.scalatest.FunSuite

class GeometryTests extends FunSuite {
  val r = new scala.util.Random()

  def randomCell(w: Double, h: Double): Coor = {
    Coor(r.nextDouble() * w, r.nextDouble() * h)
  }

  test("Inside test - square") {
    val p = Poly(List(
      Coor(0, 0),
      Coor(1, 0),
      Coor(1, 1),
      Coor(0, 1)
    ))

    val res = (1 to 100000).map(x => randomCell(2, 2)).map(Geometry.inside(p, _)).count(identity)

    println(res)
    assert(res >= 24800 && res <= 25200)
  }

  test("Inside test - concave") {
    val p = Poly(List(
      Coor(0, 0),
      Coor(1, 0),
      Coor(0.25, 0.25),
      Coor(0, 1)
    ))

    val res = (1 to 100000).map(x => randomCell(1, 1)).map(Geometry.inside(p, _)).count(identity)

    println(res)
    assert(res >= 24800 && res <= 25200)
  }
}
