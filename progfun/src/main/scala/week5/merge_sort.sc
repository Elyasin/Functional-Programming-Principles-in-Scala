object merge_sort {

  def mergeSort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {

      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, _) => ys
          case (_, Nil) => xs
          case (x :: xs1, y :: ys1) =>
            if (ord.lt(x, y)) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }

      val (fst, snd) = xs.splitAt(n)
      merge(mergeSort(fst), mergeSort(snd))
    }
  }

  val nums = List(2, -4, 5, 7, 1)
  mergeSort(nums)

  val fruits = List("apple", "pineapple", "orange", "banana")
  mergeSort(fruits)
}