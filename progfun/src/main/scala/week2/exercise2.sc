import scala.math.abs

object exercise2 {

  val tolerance = 0.0001

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(f(next), next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  def isCloseEnough(x: Double, y: Double) =
    abs(x - y) / x < tolerance

  fixedPoint(x => 1 + x / 2)(1)

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  //def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1)
  def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1)

  sqrt(2)
}