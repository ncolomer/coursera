package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._
import patmat.Huffman.Leaf
import patmat.Huffman.Fork

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

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine of singleton leaf list") {
    val leaflist = List(Leaf('e', 1))
    assert(combine(leaflist) === leaflist)
  }

  test("check secret") {
    val expected = List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
    val actual = decodedSecret
    assert(expected === actual)
  }

  test("create code tree 1") {
    val codeTree = createCodeTree(string2Chars("ffrfl"))
    val expected = makeCodeTree(makeCodeTree(Leaf('l', 1), Leaf('r', 1)), Leaf('f', 3))
    assert(expected === codeTree)
  }

  test("create code tree 2") {
    val codeTree = createCodeTree(string2Chars("eneneoue"))
    val expected = makeCodeTree(makeCodeTree(makeCodeTree(Leaf('o', 1), Leaf('u', 1)), Leaf('n', 2)), Leaf('e', 4))
    assert(expected === codeTree)
  }

  test("encode and decode a very short text, should be identity") {
    new TestTrees {
      val input = "ab".toList
      val encoded = encode(t1)(input)
      val decoded = decode(t1, encoded)
      assert(decoded === input)
    }
  }

  test("encode and decode a short text, should be identity") {
    new TestTrees {
      val input = "dba".toList
      val encoded = encode(t2)(input)
      val decoded = decode(t2, encoded)
      assert(decoded === input)
    }
  }

  test("quickEncode and decode a very short text, should be identity") {
    new TestTrees {
      val input = "ab".toList
      val encoded = quickEncode(t1)(input)
      val decoded = decode(t1, encoded)
      assert(decoded === input)
    }
  }

  test("quickEncode and decode a short text, should be identity") {
    new TestTrees {
      val input = "dba".toList
      val encoded = quickEncode(t2)(input)
      val decoded = decode(t2, encoded)
      assert(decoded === input)
    }
  }

  test("encode and decode using wikipedia sample") {
    // Given
    val input = "this is an example of a huffman tree".toList
    val codeTree = createCodeTree("this is an example of a huffman tree".toList)
    // When
    val encoded = encode(codeTree)(input)
    val decoded = decode(codeTree, encoded)
    // Then
    assert(decoded === input)
  }

  test("encode and decode using wikipedia sample and another phrase") {
    // Given
    val input = "this is an other phrase sample".toList
    val codeTree = createCodeTree("this is an example of a huffman tree".toList)
    // When
    val encoded = encode(codeTree)(input)
    val decoded = decode(codeTree, encoded)
    // Then
    assert(decoded === input)
  }

  test("quickEncode and decode using wikipedia sample") {
    // Given
    val input = "this is an example of a huffman tree".toList
    val codeTree = createCodeTree("this is an example of a huffman tree".toList)
    // When
    val encoded = quickEncode(codeTree)(input)
    val decoded = decode(codeTree, encoded)
    // Then
    assert(decoded === input)
  }

  test("quickEncode and decode using wikipedia sample and another phrase") {
    // Given
    val input = "this is an other phrase sample".toList
    val codeTree = createCodeTree("this is an example of a huffman tree".toList)
    // When
    val encoded = quickEncode(codeTree)(input)
    val decoded = decode(codeTree, encoded)
    // Then
    assert(decoded === input)
  }

}
