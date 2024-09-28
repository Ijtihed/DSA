// Scala 3 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University
 * This file is only for your personal use in the Aalto course CS-A1140.
 * Distribution of any parts of this file in any form,
 * including posting to public or shared forums,
 * storing in public or shared repositories,
 * is *not allowed*,
 * and constitutes a violation of the code of conduct of the course.
 * In addition, entering any parts of this file or the assignment instructions
 * into an AI tool, or to otherwise distributing them to external parties,
 * is *not allowed*.
 */

package tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import java.nio.charset.StandardCharsets

import huffman._

class HuffmanSpec extends AnyFlatSpec with Matchers:

  def textToBytes(text: String): Vector[Byte] = 
    text.getBytes(StandardCharsets.UTF_8).toVector

  def bytesToText(bytes: Vector[Byte]): String = 
    new String(bytes.toArray, "utf-8")


  "The buildTree method" should "work correctly" in {
    val tests = List(
      (Vector[Byte](1,2,2,3,3,3,3), 10)
      ,(textToBytes("ababccc"), 11)
      ,(textToBytes("abcd"), 8)
      ,(textToBytes("abcccddd"), 15)      
    )
    for (bytes, correctEncodingLength) <- tests do
      val tree = buildTree(bytes)
      withClue(s"On $bytes the buildTree method produced the tree $tree:") {
        withClue("Illegal symbol set in the leafs:") {
          tree.symbols should be (bytes.toSet)
        }
        withClue("Invalid size:") {
          tree.size should be (2*bytes.toSet.size - 1)
        }
        withClue("Invalid encoding size:") {
          tree.encodingLength should be (correctEncodingLength)
        }
      }
  }


  "The getTable method" should "work correctly" in {
    // List of (tree, encoding table) pairs
    val tests = List[(Node,Map[Byte,Vector[Boolean]])](
      (Internal(Leaf(98,1),Leaf(97,2)),
       Map(98.toByte -> Vector(false), 97.toByte -> Vector(true))
      )
      ,(Internal(Leaf(98,8),Leaf(97,9)),
        Map(98.toByte -> Vector(false), 97.toByte -> Vector(true))
      )
      ,(Internal(Leaf(97,1),Internal(Leaf(98,1),Leaf(99,1))),
        Map(98.toByte -> Vector(true, false), 97.toByte -> Vector(false), 99.toByte -> Vector(true, true))
      )
      ,(Internal(Internal(Leaf(98,1),Leaf(99,1)), Leaf(97,1)),
        Map(98.toByte -> Vector(false, false), 99.toByte -> Vector(false,true), 97.toByte -> Vector(true))
      )
      ,(Internal(Leaf(100,4),
                 Internal(Leaf(99,3),Internal(Leaf(98,2),Leaf(97,2)))),
        Map(98.toByte -> Vector(true, true, false),
            100.toByte -> Vector(false), 97.toByte -> Vector(true, true, true),
            99.toByte -> Vector(true, false))
      )
      ,(Internal(Leaf(0xd0.toByte,5),
                 Internal(Leaf(0x71.toByte,2),Leaf(0x68.toByte,4))),
        Map(0xd0.toByte -> Vector(false), 0x71.toByte -> Vector(true, false),
            0x68.toByte -> Vector(true, true))
      )
    )
    for (tree, correct) <- tests do
      withClue(s"On the tree $tree: ") {
        val table = huffman.getTable(tree)
        table should be (correct)
      }
  }


  "The encodeTree method" should "work correctly" in {
    // List of (tree, correct encoding) pairs
    val tests = List(
      (Internal(Leaf(0xd0.toByte,3),Internal(Leaf(0x71.toByte,1),Leaf(0x68.toByte,1))),
       "01110100000101110001101101000"
      )
      ,(Internal(Leaf(0x00.toByte,3),Internal(Leaf(0x02.toByte,1),Leaf(0x03.toByte,1))),
        "01000000000100000010100000011"
      )
      ,(Internal(Internal(Leaf(0x00.toByte,1),Leaf(0x02.toByte,2)),Leaf(0x03.toByte,3)),
        "00100000000100000010100000011"
      )
      ,(Internal(Internal(Leaf(0x00.toByte,2),Leaf(0x02.toByte,2)),
                 Internal(Leaf(0x03.toByte,3),Leaf(0xff.toByte,3))),
        "001000000001000000100100000011111111111"
      )
    )
    for (tree, bitsString) <- tests do
      withClue(s"On the tree $tree:") {
        val bits = bitsString.map(_ == '1').toVector
        val result = huffman.encodeTree(tree)
        result should be (bits)
      }
  }


  "The decodeTree method" should "work correctly" in {
    // List of (encoding, correct tree) pairs
    val tests = List(
      ("01110100000101110001101101000",
        Internal(Leaf(0xd0.toByte,0),Internal(Leaf(0x71.toByte,0),Leaf(0x68.toByte,0))))
      ,("01000000000100000010100000011",
        Internal(Leaf(0x00.toByte,0),Internal(Leaf(0x02.toByte,0),Leaf(0x03.toByte,0))))
      ,("00100000000100000010100000011",
        Internal(Internal(Leaf(0x00.toByte,0),Leaf(0x02.toByte,0)),Leaf(0x03.toByte,0)))
      ,("001000000001000000100100000011111111111",
        Internal(Internal(Leaf(0x00.toByte,0),Leaf(0x02.toByte,0)),
                 Internal(Leaf(0x03.toByte,0),Leaf(0xff.toByte,0)))
      )
    )
    for (bitsString, correctTree) <- tests do
      val bits = bitsString.map(_ == '1').toVector
      withClue(s"On the encoded bit sequence $bits:") {
        val tree = huffman.decodeTree(bits)
        tree should be (correctTree)
      }
  }


  "Encoding and decoding" should "work correctly" in {
    val texts = List(("this is an example of a huffman tree", 159, 135)
                     ,("aaaaaaaaaabbbbbbbbbb",19,20)
                     ,("Typical English text contains lots of e's and i's but also other characters are quite common. This may apply to other languages as well, even with non-ASCII characters like ä and ö.", 379, 826)
                     ,("In this exercise, your task is to implement code that builds Huffman trees and gets the codes for the symbols in the tree.",259,505)
                   )
    for (text, treeEncLength, textEncLength) <- texts do
      withClue(s"""On text \"$text\":""") {
        val (encodedTree, encodedText) = huffman.encode(textToBytes(text))
        println("Encoding text: \""+text+"\"")
        println(" Original length:   "+(text.getBytes.length*8))
        println(s" Compressed length: ${encodedTree.length} + ${encodedText.length} = ${encodedTree.length + encodedText.length}")
        encodedTree.length should be (treeEncLength)
        encodedText.length should be (textEncLength)
        val decoded = bytesToText(huffman.decode(encodedTree, encodedText))
        decoded should be (text)
      }
  }
end HuffmanSpec
