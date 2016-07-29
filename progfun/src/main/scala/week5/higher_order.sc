object higher_order {

  def squareList1(xs: List[Int]): List[Int] =
    xs match {
      case Nil => xs
      case y :: ys => (y*y) :: squareList1(ys)
    }

  def squareList2(xs: List[Int]): List[Int] =
    xs map (y => y * y)

}