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
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("can count") {
    new TestTrees {
      assert(times("aaaaaabbbaacdefgh".toList) === List(('a', 8), ('b', 3), ('c',1), ('d',1), ('e',1), ('f',1), ('g',1), ('h',1)))
    }
  }

    test("create code tree") {
    new TestTrees {
      assert(createCodeTree("aaaabaaabbbccc".toList) === Fork(Fork(Leaf('c',3),Leaf('b',4),List('c', 'b'),7),Leaf('a',7),List('c','b', 'a'),14))
    }
  }

    
  test("size 1") {
    new TestTrees {
      assert(singleton(List(Leaf('a', 2))) === true)
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

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("combine of some leaf list preserves weight") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(combine(leaflist) === List(Leaf('x', 4), Fork(Leaf('e', 2), Leaf('t', 3), List('e', 't'), 5)))
  }

  test("combine until singleton") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) === List(Fork(Leaf('x', 4), Fork(Leaf('e', 2), Leaf('t', 3), List('e', 't'), 5), List('x', 'e', 't'), 9)))
  }

  test("making a tree for realz") {
    val chars = List('x', 't', 'e', 'e', 'x', 'x', 'x', 't', 't')
    val tree = createCodeTree(chars)
    assert(tree === Fork(Leaf('x', 4), Fork(Leaf('e', 2), Leaf('t', 3), List('e', 't'), 5), List('x', 'e', 't'), 9))
    assert(decode(tree, List(1, 0, 0, 1, 1)) === "ext".toList)
    assert(encode(tree)("ext".toList) === List(1, 0, 0, 1, 1))
  }

  test("convert tree to table") {
    new TestTrees {
      val chars = List('x', 't', 'e', 'e', 'x', 'x', 'x', 't', 't')
      val tree = createCodeTree(chars)
      assert(convert(tree) ===List(('x',List(0)), ('e',List(1, 0)), ('t',List(1, 1))))
    }
  }

    test("test code bit") {
    new TestTrees {
      val codeTable =List(('x',List(0)), ('e',List(1, 0)), ('t',List(1, 1)))
      assert(codeBits(codeTable)('e') === List(1,0))
    }
  }

      test("test code text") {
    new TestTrees {
      val chars = List('x', 't', 'e', 'e', 'x', 'x', 'x', 't', 't')
      val tree = createCodeTree(chars)
      assert(quickEncode(tree)("ext".toList) === List(1,0,0,1,1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("french secret") {
    new TestTrees {
      assert(decodedSecret === "huffmanestcool".toList)
    }
  }
}
