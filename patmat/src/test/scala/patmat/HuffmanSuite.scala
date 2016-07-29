package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val t3 = Fork(Fork(Fork(Fork(Leaf('d', 1), Leaf('c', 1), List('d', 'c'), 2), Leaf('r', 2), List('d', 'c', 'r'), 4),
      Leaf('b', 2), List('d', 'c', 'r', 'b'), 6), Leaf('a', 5), List('d', 'c', 'r', 'b', 'a'), 11)

    val b3 = List(1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1)

    val text = List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("times(List('a', 'b', 'a'))") {
    assert(
      times(List('a', 'b', 'a')) === List(('a', 2), ('b', 1)) ||
        times(List('a', 'b', 'a')) === List(('b', 1), ('a', 2))
    )
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


  test("singleton on one tree in list must return true") {
    assert(singleton(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3))) === true)
  }

  test("singleton on two trees in list must return false") {
    assert(singleton(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4))) === false)
  }

  test("combine a leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("until must combine a list of code trees until only one tree is left") {
    val leaflist = List(Leaf('a', 1), Leaf('b', 2), Leaf('c', 3), Leaf('d', 4))
    assert(
      until(singleton, combine)(leaflist)
        ===
        List(Fork(Leaf('d', 4), Fork(Leaf('c', 3), Fork(Leaf('a', 1), Leaf('b', 2), List('a', 'b'), 3),
          List('c', 'a', 'b'), 6), List('d', 'c', 'a', 'b'), 10))
    )
  }

  test("create code tree") {
    val listchars = List('a', 'b', 'r', 'a', 'c', 'a', 'd', 'a', 'b', 'r', 'a')
    assert(createCodeTree(listchars)
      ===
      Fork(Leaf('a', 5), Fork(Fork(Leaf('d', 1), Leaf('c', 1), List('d', 'c'), 2), Fork(Leaf('r', 2),
        Leaf('b', 2), List('r', 'b'), 4), List('d', 'c', 'r', 'b'), 6), List('a', 'd', 'c', 'r', 'b'), 11)
    )
  }

  test("decode") {
    new TestTrees {
      assert(decode(t3, b3) === List('a', 'b', 'r', 'a', 'c', 'a', 'd', 'a', 'b', 'r', 'a'))
    }
  }

  test("decode secret") {
    new TestTrees {
      assert(decode(frenchCode, secret) === text)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode encoded test with frenchCode") {
    val code = List(1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1)
    assert(decode(frenchCode, code) === string2Chars("encoreuntextetressecret"))
  }

  test("very simple codebits") {
    val table: CodeTable = List(('a', List(0)), ('b', List(1)))
    assert(codeBits(table)('a') === List(0))
  }

  test("convert code tree to code table") {
    new TestTrees {
      val table =
        List(('d', List(0, 0, 0, 0)), ('c', List(0, 0, 0, 1)), ('r', List(0, 0, 1)), ('b', List(0, 1)), ('a', List(1)))
      assert(convert(t3) === table)
    }
  }

  test("quick encoding") {
    new TestTrees {
      assert(quickEncode(frenchCode)(text) === secret)
    }
  }

  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("encode and quickEncode must return same result for same code and text") {
    new TestTrees {
      assert(quickEncode(frenchCode)(text) === encode(frenchCode)(text))
    }
  }

}
