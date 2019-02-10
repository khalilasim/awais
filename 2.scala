
import breeze.linalg._

import breeze.numerics._
class s0l {

  def Sphere(x :scala.collection.immutable.Vector[Double]) : Double = {

    val power = x.map(math.pow(_, 2))

    return  sum(power)
  }

  def Ackley(x: Vector[Double]): Double =
  {
    val a=20
    val b=0.2
    val c=2 * constants.Pi
    var cols =x.length;
    var inverseCols = 1.0/cols
    println(inverseCols)
    val power = x.map(math.pow(_, 2))
    var sum1  = sum(power)
    println(sum1)
    var y =  -a * exp(-b * sqrt(inverseCols * sum1)) -  exp(inverseCols * sum(cos(c * x))) +a + constants.E
    y
  }

}
