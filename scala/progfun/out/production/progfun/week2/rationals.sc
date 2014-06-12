

object rationals {

  class Rational(x: Int, y: Int) {
    require(y > 0, "denominator must be positive")
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    private val gcd = gcd(x, y)
    val numer = x / gcd
    val denom = y / gcd
    def +(that: Rational) =
      new Rational(
        this.numer * that.denom + that.numer * this.denom,
        this.denom * that.denom)
    def unary_- = new Rational(-numer, denom)
    def -(that: Rational) = this + -that
    def <(that: Rational) = this.numer * that.denom < that.numer * this.denom
    def max(that: Rational) = if (this < that) that else this
    def simplify = {

    }
    override def toString = s"${numer}/${denom}"
  }

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  x - y - z

  y + y

}

