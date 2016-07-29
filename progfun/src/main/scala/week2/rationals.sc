object rationals {

  val x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)

  x.numer
  x.denom

  x - y - z
  y + y
  x < y
  x max y



  class Rational(x: Int, y: Int) {

    require(y != 0, "denominator must be non zero")

    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)

    def numer = x
    def denom = y

    def < (that: Rational) =
      this.numer * that.denom < that.numer * this.denom

    def max(that: Rational) =
      if (this < that) that else this

    def + (that: Rational) =
      new Rational(
        numer * that.denom + denom * that.numer,
        denom * that.denom
      )

    override def toString = {
      val g = gcd(numer, denom)
      numer/g + "/" + denom/g
    }

    def unary_- : Rational = new Rational(-numer, denom)

    def - (that: Rational) = this + -that

  }

  def makeString(r: Rational) =
    r.numer + "/" + r.denom

  def addRational(r: Rational, s: Rational) =
    new Rational(r.numer * s.denom + s.numer * r.denom,
      r.denom * s.denom)

  makeString(addRational(new Rational(1, 2), new Rational(2, 3)))


}
