
import scala.math._

def multiply(x: Int, y: Int): Int = {
  def n = floor(log10(x)) + 1;
  if (n == 1) x * y
  else {
    def _10eN2 = pow(10, n / 2).toInt
    def _10eN = pow(10, n).toInt
    def a = floor(x / _10eN2).toInt
    def b = x % _10eN2
    def c = floor(y / _10eN2).toInt
    def d = y % _10eN2
    println("n=", n, "a=", a, "b=", b, "c=", c, "d=", d)
    def ac = multiply(a, c)
    def bd = multiply(b, d)
    def adbc = multiply(a + b, c + d) - ac - bd
    ac * _10eN + adbc * _10eN2 + bd
  }
}
floor(log10(5)) + 1
multiply(5678, 1234)
//assert(multiply(5678, 1234) == 7006652, "5678*1234=7006652")
