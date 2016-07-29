object exercise {

  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys => reverse(ys) ++ List(y)
  }

  def removeAt[T](n: Int, xs: List[T]) =
    (xs take n) ::: (xs drop n + 1)

  removeAt(1, List('a', 'b', 'c', 'd'))


  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => List()
    case (x: List[_]) :: xs1 => flatten(x) ::: flatten(xs1)
    case x :: xs1 => x :: flatten(xs1)
  }

  flatten(List(List(1, 1), 2, List(3, List(5, 8))))
}