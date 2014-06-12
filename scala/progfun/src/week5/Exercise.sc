

def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (x < y) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

val nums = List(2, -4, 5, 7, 1)
msort(nums)
def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (packed, rest) = xs span (y => y == x)
    packed :: pack(rest)
  }
}

def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs) map (list => (list.head, list.length))
}

val list = List("a", "a", "a", "b", "c", "c", "a")
pack(list)
encode(list)



