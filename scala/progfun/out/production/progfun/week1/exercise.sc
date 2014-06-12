

object exercise {

  def factorial(n: Int): Int =
    if (n == 0) 1 else n * factorial(n - 1)

  factorial(4)

  def factorial2(n: Int): Int = {
    def rec(i: Int, p: Int): Int =
      if (i == 0) p else rec(i - 1, p * i)
    rec(n, 1)
  }

  factorial2(4)
}
