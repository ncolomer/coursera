import scala.util.Random._
trait data {
  val array = Array(3, 8, 2, 5, 1, 4, 7, 6)
  val result = Array(1, 2, 3, 4, 5, 6, 7, 8)
}
new data {
  // Random pivot picker
  quickSort(n => nextInt(n))(array)
  println(array.mkString(","))
}

new data {
  // First element pivot picker
  quickSort(n => 0)(array)
  println(array.mkString(","))
}

def quickSort(pivotPicker: Int => Int)(array: Array[Int]) {
  def partition(from: Int, to: Int): Int = {
    def swap(i: Int, j: Int) =
      if (i != j) {
        val tmp = array(i)
        array(i) = array(j)
        array(j) = tmp
      }
    // Pick a pivot and move it as first element
    swap(from, from + pivotPicker(to - from))
    // Get the pivot value
    val pivot = array(from)
    var i = from + 1
    // Partition the array
    for (j <- from + 1 until to if array(j) < pivot) {
      swap(i, j)
      i += 1
    }
    // Replace pivot at its final position
    val finalPivotIndex = i - 1
    swap(from, finalPivotIndex)
    finalPivotIndex
  }
  def recursiveSort(from: Int, to: Int): Unit =
    if (to - from > 1) {
      val pivotIndex = partition(from, to)
      recursiveSort(from, pivotIndex)
      recursiveSort(pivotIndex + 1, array.length)
    }
  recursiveSort(0, array.length)
}