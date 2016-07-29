package week4


abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet =
    new NonEmpty(x, new Empty, new Empty)

  def union(other: IntSet): IntSet = other

  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem

  override def toString = "{" + left + elem + right + "}"
}

trait List[+T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]

  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)

}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false

}

object Nil extends List[Nothing] {
  def isEmpty = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")

}

object List {
  // List(1,2) = List.apply(1, 2)
  def apply[T](x: T, y: T): List[T] = new Cons(x, new Cons(y, Nil))

  // List(1) = List.apply(1)
  def apply[T](x: T): List[T] = new Cons(x, Nil)

  // List() = List.apply()
  def apply[T](): List[T] = Nil
}

object test {
  val x: List[String] = Nil

  def f(xs: List[NonEmpty], x: Empty): List[IntSet] = xs prepend x
}
