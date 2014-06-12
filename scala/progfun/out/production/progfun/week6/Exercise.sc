

def isPrime(n: Int): Boolean = 2 until n forall (i => n % i > 0)
2 to 10 foreach (i => if (isPrime(i)) println(s"${i} is prime"))
def findPairs(n: Int) = for {
  i <- 1 until n
  j <- 1 until i
  if (isPrime(i + j))
} yield (i, j)
findPairs(6).foreach(pair => println(pair))
def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
  val products = for ((a, b) <- xs zip ys) yield a * b
  products.sum
}
scalarProduct(List(1, 2, 3), List(1, 2, 3))












