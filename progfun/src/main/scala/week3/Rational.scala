package week3


class Rational(x: Int, y: Int) {

  require(y != 0, "denominator must be non zero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def numer = x

  def denom = y

  def <(that: Rational) =
    this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) =
    if (this < that) that else this

  def +(that: Rational) =
    new Rational(
      numer * that.denom + denom * that.numer,
      denom * that.denom
    )

  override def toString = {
    val g = gcd(numer, denom)
    numer / g + "/" + denom / g
  }

  def unary_- : Rational = new Rational(-numer, denom)

  def -(that: Rational) = this + -that

}



