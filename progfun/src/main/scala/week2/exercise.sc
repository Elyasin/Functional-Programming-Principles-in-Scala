object exercise {

  def factorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)
    loop(1, n)
  }

  factorial(4)

  //  def sum(f: Int => Int)(a: Int, b: Int): Int =
  //    if (a > b) 0
  //    else f(a) + sum(f)(a + 1, b)

  //  def product(f: Int => Int)(a: Int, b: Int): Int =
  //    if (a > b) 1 else f(a) * product(f)(a + 1, b)

  def fact(a: Int) = product(x => x)(1, a)

  fact(5)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

  def product(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x * y, 1)(a, b)

  product(x => x * x)(3, 4)

  def sum(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x + y, 0)(a, b)

  sum(x => x)(1, 5)
}

