object pairs {


  def isPrime(n: Int) = (2 until n) forall (n % _ != 0)


  val n = 7
  (1 until n) flatMap (i =>
    (1 until i) map (j => (i, j))) filter (pair => isPrime(pair._1 + pair._2))


  case class Person(name: String, age: Int)

  val persons = List(Person("Ely", 35), Person("Nur", 28), Person("Sara", 6))
  for (p <- persons if p.age > 20) yield p.name
  persons filter (p => p.age >= 20) map (p => p.name)


  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)

  def scalarProduct(xs: List[Double], ys: List[Double]): Double =
    (for ((x, y) <- xs zip ys) yield x * y).sum
}