package scala.week1


/**
 * Created by Nicolas on 30/04/2014.
 */
object InversionCounting {

  def countInversion(array: Array[Int]): Long = {
    def mergeAndCountSplitInv(b: Array[Int], c: Array[Int]): (Array[Int], Long) = {
      val d = new Array[Int](b.length + c.length)
      var (i, j, inv) = (0, 0, 0l)
      for (k <- d.indices) {
        if (j == c.length) {
          // No more item in c => fill with b
          d(k) = b(i)
          i += 1
        } else if (i == b.length) {
          // no more item in b => fill with c
          d(k) = c(j)
          j += 1
        } else if (b(i) <= c(j)) {
          d(k) = b(i)
          i += 1
        } else if (b(i) > c(j)) {
          d(k) = c(j)
          j += 1
          inv += b.length - i
        }
      }
      (d, inv)
    }
    def sortAndCount(array: Array[Int]): (Array[Int], Long) = {
      val n = array.length
      if (n == 1) (array, 0)
      else {
        val (left, right) = array.splitAt(n / 2)
        val (b, x) = sortAndCount(left)
        val (c, y) = sortAndCount(right)
        val (d, z) = mergeAndCountSplitInv(b, c)
        (d, x + y + z)
      }
    }
    sortAndCount(array)._2
  }

}
