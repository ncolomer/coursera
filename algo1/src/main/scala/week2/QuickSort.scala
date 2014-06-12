package scala.week2

object QuickSort {

  def sort(peekPivot: Array[Int] => Int)(array: Array[Int]): Long = {
    var comparisons: Long = 0
    def partition(from: Int, to: Int): Int = {
      def swap(i: Int, j: Int) = if (i != j) {val tmp = array(i); array(i) = array(j); array(j) = tmp}
      // Pick a pivot and move it as first element
      swap(from, from + peekPivot(array.slice(from, to)))
      // Get the pivot value
      val pivot = array(from)
      // Partition (sort around pivot) the array
      val i = (from + 1 until to).foldLeft[Int](from + 1) {
        (i, j) => if (array(j) < pivot) {swap(i, j); i + 1} else i
      }
      // Replace pivot at its final position
      val finalPivotIndex = i - 1
      swap(from, finalPivotIndex)
      finalPivotIndex
    }
    def recursiveSort(from: Int, to: Int): Unit =
      if (to - from > 1) {
        // Increment number of comparisons
        comparisons += to - from - 1
        val pivotIndex = partition(from, to)
        recursiveSort(from, pivotIndex)
        recursiveSort(pivotIndex + 1, to)
      }
    recursiveSort(0, array.length)
    comparisons
  }

}
