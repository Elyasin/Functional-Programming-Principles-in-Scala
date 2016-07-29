object exercise {

  val str = "Robert"
  val occ =
    (for ((n, lst) <- str.groupBy(c => str.count(_.toLower == c.toLower)); c <- lst)
      yield (c.toLower, n)).toList.sorted

  val strList = List("a", "b", "c")
  strList.foldLeft(" ")((z, a) => z.concat(a))

  (str flatMap (c => List('.', c)))

  List("Every", "student", "likes", "Scala").groupBy((element: String) => element.length)

  val m1 = Map('a' -> 3, 'b' -> 2, 'c' -> 1)
  val m2 = Map('b' -> 1, 'c' -> 1)
  m1 withDefaultValue(0)
  m1.get('x')

  m2.foldLeft(m1)(
    (z: Map[Char, Int], x: (Char, Int)) =>
      if (z(x._1) == x._2) (z - x._1)
      else if (z.get(x._1) == None) z
      else z.updated(x._1, z(x._1) - x._2)
  )


}