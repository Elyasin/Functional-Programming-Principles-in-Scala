object list_fun {

  val nums = List(2, -4, 5, 7, 1)
  val fruits = List("apple", "pineapple", "orange", "banana")

  nums filter (x => x > 0)
  nums filterNot (x => x > 0)
  nums partition (x => x > 0)

  nums takeWhile (x => x > 0)
  nums dropWhile (x => x > 0)
  nums span (x => x > 0)


  val data = List("a", "a", "a", "b", "c", "c", "a")

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
      val (first, rest) = xs span (y => x == y)
      first :: pack(rest)
    }
  }

  pack(data)

  def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (ys => (ys.head, ys.size))

  encode(data)

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys) (_ :: _)

  concat(List(1, 2, 3), List(4, 5, 6))

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]())( (a,z) => f(a) :: z )

  mapFun[String, String](data, x => x.capitalize)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)( (a,z) => z + 1 )

  lengthFun(data)
}