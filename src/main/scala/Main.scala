package main
import breeze.linalg._, eigSym.EigSym

object Main extends App {
  override def main(args: Array[String]): Unit = {
//    val A = DenseMatrix((9.0,0.0,0.0),(0.0,82.0,0.0),(0.0,0.0,25.0))
    val A = DenseMatrix((1.0,2.0,3.0),(1.0,1.0,2.0),(4.0,3.0,1.0))
    val z = A * A.t



    println(z)
//    val EigSym(lambda, evs) = eigSym(A)
//    println(lambda)
//    println(evs)
  }
}

object Tsne {
  def product(f: List[Double], s: List[Double]): Double = {
    val z = (f zip s) map {
      case (a: Double, b: Double) => a * b
    }

    z.sum
  }

  def hBeta(d: List[Double], beta: Double = 1.0) = {
    val p = d.map(x => Math.exp(-x * beta))
    val sumP = p.sum
    val h = Math.log(sumP) + beta * product(d, p) / sumP
    (h, p.map(x => x / sumP))
  }

  def x2p(x: DenseMatrix[Double], tol: Double = 1e-5, perplexity: Double = 30.0) {

  }

}