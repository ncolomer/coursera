package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (r == 0 || c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def lookup(chars: List[Char], count: Int): Boolean =
      if (count < 0) false // A parenthesis was closed whereas none was opened before
      else if (chars.isEmpty) (count == 0) // No more chars, is the count good?
      else lookup(chars.tail, chars.head match {
        case '(' => count + 1
        case ')' => count - 1
        case _ => count
      })
    lookup(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def lookup(left: Int, coins: List[Int]): Int = {
      if (left == 0) 1 // Found a way to make change!
      else if (coins.isEmpty) 0 // No more coins available
      else if (coins.head > left) lookup(left, coins.tail)
      else lookup(left, coins.tail) + // Try with the lower coins
        lookup(left - coins.head, coins) // Try with this coin
    }
    lookup(money, coins.sortWith(_ > _)) // Ensure coins are sorted desc
  }
}
