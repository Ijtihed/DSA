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

package huffman

/*
 * Do not modify the code below.
 * Or if you do and add some debugging code etc, pay attention to the fact that
 * the grading process only uses an unmodified version of this file.
 */

/**
 * The base class for the nodes in Huffman trees.
 */
abstract class Node:
  /** Is this node a leaf? */
  def isLeaf = false

  /**
   * In a leaf node: the number of occurrences of a symbol.
   * In an internal node: The sum of all the freqs in
   *   the leaf nodes of the subtree rooted at the node.
   */
  def freq: Int

  /**
   * The number of nodes in the subtree rooted at this node
   * (which is included in the count).
   */
  def size: Int = this match
    case Leaf(_, _) => 1
    case Internal(left, right) => left.size + right.size + 1

  /**
   * The set of byte symbols occurring in the leafs
   * of the subtree rooted at this node.
   */
  def symbols: Set[Byte] =
    val result = collection.mutable.Set[Byte]()
    def inner(node: Node): Unit = node match
      case Leaf(symbol, _) => result += symbol
      case Internal(left, right) =>
        inner(left)
        inner(right)
    inner(this)
    result.toSet

  /**
   * Returns the number of bits in the encoding when
   * this tree is applied on an input sequence that consists of the symbols
   * appearing in the leafs as many times as the leaf frequencies indicate.
   */
  def encodingLength: Int =
    def inner(node: Node, depth: Int): Int = node match
      case Leaf(_, freq) =>
        assert(freq > 0)
        depth * freq
      case Internal(left, right) =>
        inner(left, depth+1) + inner(right, depth+1)
    inner(this, 0)

  /**
   * Returns a prettier multiline string presentation of the tree.
   */
  def prettyString: String =
    val result = new collection.mutable.StringBuilder()
    def inner(node: Node, indent: Int): Unit = node match
      case Leaf(symbol, freq) =>
        result ++= "  "*indent + f"Leaf(0x$symbol%02x, $freq)"+"\n"
      case Internal(left, right) =>
        result ++= "  "*indent + "Internal\n"
        inner(left, indent+1)
        inner(right, indent+1)
    inner(this, 0)
    result.toString
end Node

/**
 * A leaf node.
 * Associates a symbol to the number of times it occurs in the input.
 */
case class Leaf(val symbol: Byte, val freq: Int) extends Node:
  override def isLeaf = true

/**
 * An internal node: the frequency is the sum of the frequencies of
 * the left and right subtrees.
 */
case class Internal(val left: Node, val right: Node) extends Node:
  val freq = left.freq + right.freq
