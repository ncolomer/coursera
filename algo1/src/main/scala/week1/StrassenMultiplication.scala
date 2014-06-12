package scala.week1

import scala.Array._

/**
 * Created by Nicolas on 01/05/2014.
 * Strassen's Subcubic Matrix Multiplication Algorithm
 */
object StrassenMultiplication {

  type Raw = Array[Array[Int]]

  abstract class Matrix(rows: Int, columns: Int) {
    require(rows > 0, s"${rows} rows is invalid")
    require(columns > 0, s"${columns} columns is invalid")

    val raw = ofDim[Int](rows, columns)
    val m = rows
    val n = columns

    def populate(values: Int*): this.type = {
      for ((value, index) <- values.zipWithIndex)
        raw(index / m)(index % n) = value
      this
    }

    def set(i: Int, j: Int, x: Int): this.type =
      if (i >= 0 && i < m && j >= 0 && j < n) {raw(i)(j) = x; this}
      else throw new IndexOutOfBoundsException

    def get(i: Int, j: Int): Int =
      if (i >= 0 && i < m && j >= 0 && j < n) raw(i)(j)
      else throw new IndexOutOfBoundsException

    override def equals(other: Any) = other match {
      case other: Matrix => this.raw.deep == other.raw.deep
      case _ => false
    }
  }

  class SimpleMatrix(rows: Int, columns: Int) extends Matrix(rows, columns)

  class SquareMatrix(size: Int) extends Matrix(size, size) {
    def +(other: SquareMatrix): SquareMatrix = {
      val matrix = new SquareMatrix(size)
      for (i <- 0 until size; j <- 0 until size)
        matrix.set(i, j, this.get(i, j) + other.get(i, j))
      matrix
    }
    def -(other: SquareMatrix): SquareMatrix = {
      val matrix = new SquareMatrix(size)
      for (i <- 0 until size; j <- 0 until size)
        matrix.set(i, j, this.get(i, j) - other.get(i, j))
      matrix
    }
    def *(other: SquareMatrix): SquareMatrix = {
      assert(this.n == other.n, "Matrices are not the same size")
      def merge(A: SquareMatrix, B: SquareMatrix, C: SquareMatrix, D: SquareMatrix): SquareMatrix = {
        val n = A.n * 2
        val s = A.n
        val m = new SquareMatrix(n)
        for (i <- 0 until n; j <- 0 until n) (i, j) match {
          case (i, j) if i < s && j < s => m.set(i, j, A.get(i, j))
          case (i, j) if i < s && j < n => m.set(i, j, B.get(i, j - s))
          case (i, j) if i < n && j < s => m.set(i, j, C.get(i - s, j))
          case (i, j) => m.set(i, j, D.get(i - s, j - s))
        }
        m
      }
      if (this.n == 1 && other.n == 1)
        new SquareMatrix(1).populate(this.get(0, 0) * other.get(0, 0))
      else {
        val A = this.sub(0, n / 2, 0, n / 2)
        val B = this.sub(0, n / 2, n / 2, n)
        val C = this.sub(n / 2, n, 0, n / 2)
        val D = this.sub(n / 2, n, n / 2, n)
        val E = other.sub(0, n / 2, 0, n / 2)
        val F = other.sub(0, n / 2, n / 2, n)
        val G = other.sub(n / 2, n, 0, n / 2)
        val H = other.sub(n / 2, n, n / 2, n)
        val P1 = A * (F - H)
        val P2 = (A + B) * H
        val P3 = (C + D) * E
        val P4 = D * (G - E)
        val P5 = (A + D) * (E + H)
        val P6 = (B - D) * (G + H)
        val P7 = (A - C) * (E + F)
        merge(
          P5 + P4 - P2 + P6,
          P1 + P2,
          P3 + P4,
          P1 + P5 - P3 - P7
        )
      }
    }
    def sub(i0: Int, i1: Int, j0: Int, j1: Int): SquareMatrix = {
      require(0 <= i0 && i0 <= size, s"i0=${i0} index is not in range [0,${size}]")
      require(i0 <= i1 && i1 <= size, s"i1=${i1} index is not in range [${i0},${size}]")
      require(0 <= j0 && j0 <= size, s"j0=${j0} index is not in range [0,${size}]")
      require(j0 <= j1 && j1 <= size, s"j1=${j1} index is not in range [${j0},${size}]")
      require(i1 - i0 == j1 - j0, s"Sub matrix (${i1 - i0},${j1 - j0}) is not square")
      new SubSquareMatrix(i1 - i0, i0, i1, j0, j1, raw)
    }
    override def equals(other: Any) = other match {
      case other: SquareMatrix => {
        for (i <- 0 until size; j <- 0 until size)
          if (this.get(i, j) != other.get(i, j)) false
        true
      }
      case _ => false
    }
    override def toString = {
      val items = for (i <- 0 until size; j <- 0 until size) yield {get(i, j)}
      items.mkString("(", ",", ")")
    }

    class SubSquareMatrix(size: Int, i0: Int, i1: Int, j0: Int, j1: Int, original: Raw) extends SquareMatrix(size) {
      override val raw = original
      override def set(i: Int, j: Int, x: Int) =
        if (i >= 0 && i < size && j >= 0 && j < size) {raw(i0 + i)(j0 + j) = x; this}
        else throw new IndexOutOfBoundsException
      override def get(i: Int, j: Int): Int =
        if (i >= 0 && i < size && j >= 0 && j < size) raw(i0 + i)(j0 + j)
        else throw new IndexOutOfBoundsException
    }

  }

}
