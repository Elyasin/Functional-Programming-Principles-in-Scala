package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == r || c == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty && count != 0) false
      else if (chars.isEmpty && count == 0) true
      else if (chars.head == '(') balanceIter(chars.tail, count + 1)
      else if (chars.head == ')' && count > 0) balanceIter(chars.tail, count - 1)
      else if (chars.head == ')' && count < 1) false
      else balanceIter(chars.tail, count)
    }
    balanceIter(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
