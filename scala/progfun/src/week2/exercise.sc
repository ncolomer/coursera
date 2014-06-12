// Sum of values
def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  loop(a, 0)
}
sum(x => x * x, 3, 5)

// Product of values
def prod(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc
    else loop(a + 1, acc * f(a))
  loop(a, 1)
}
prod(x => x)(3, 5)
prod(x => x * x)(3, 4)

// Factorial in terms of product
def factorial(n: Int) = prod(x => x)(1, n)
factorial(3)
factorial(4)
factorial(5)

// Generalize sum and product
def op(g: (Int, Int) => Int)(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc
    else loop(a + 1, g(acc, f(a)))
  loop(a, 0)
}
def sumOfSquares = op((a, b) => a + b)(x => x * x) _
sumOfSquares(3, 5)