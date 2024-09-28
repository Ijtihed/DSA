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

import collection.mutable.ArrayBuffer

/*
 * NOTE: only modify the methods marked with ???.
 *  In other words, do NOT touch the other methods
 *   (the grading process will use unmodified versions of them).
 */



/**
 * Encode the sequence of bytes.
 * Returns a pair consisting of
 * - the encoding of a tree (needed for decoding), and
 * - the encoding of the sequence according to the tree.
 * The results are sequences of bits: for manipulation simplicity,
 * bit sequences are here represented with Vectors of Booleans while
 * a more compact way would be to represent them with Arrays of Bytes or Ints.
 */
def encode(bytes: Vector[Byte]): (Vector[Boolean],Vector[Boolean]) =
  // Build the Huffman tree for the input byte sequence
  val tree = buildTree(bytes)
  // Encode the input byte sequence
  val encodedBytes = {
    // Get the encoding table mapping bytes to bit sequences
    val table = getTable(tree)
    // Mutable buffer for building the result
    val result = new ArrayBuffer[Boolean]()
    // Encode each input sequence byte
    for b <- bytes do
      result ++= table(b)
    // The result is converted to an immutable Vector
    result.toVector
  }
  // Encode the Huffman tree in a bit sequence
  val encodedTree = encodeTree(tree)
  // Return the encoded tree and byte sequence
  (encodedTree, encodedBytes)
end encode


/**
 * Reverse of encode.
 * Input consists of
 * - the encoding of a Huffman tree and
 * - the encoding of a byte sequence obtained with the tree.
 * Returns the byte sequence.
 */
def decode(encodedTree: Vector[Boolean], encodedBytes: Vector[Boolean]): Vector[Byte] =
  val tree = decodeTree(encodedTree)
  val result = new ArrayBuffer[Byte]()
  val iter = encodedBytes.iterator
  while iter.hasNext do
    // Start from the root node
    var node = tree
    var done = false
    while !done do
      node match
        case Internal(left, right) =>
          // Read the next bit in the encoded byte sequence
          val bit = iter.next()
          // And branch by its value: 0 to the left, 1 to the right
          if bit then node = right
          else node = left
        case Leaf(byte, _) =>
          // Reached a leaf, output the byte
          result += byte
          done = true
    end while
  end while
  result.toVector
end decode

/**
 * Build a Huffman tree for the byte sequence.
 */
def buildTree(bytes: Vector[Byte]): Node =
    /* Hints:
   * First compute the frequencies (number of occurrences)
   *  of the bytes occurring in the sequence.
   * Then associate each byte with non-zero occurrence with
   *  a leaf node and put the nodes in a priority queue.
   * Finally, build the tree by taking the two nodes with smallest
   *  frequencies from the queue, make a new internal node
   *  having them as the children (make the node with smaller
   *  frequency to be the left child), and insert the new node in
   *  the queue. Repeat until the queue has only one node
   *  (the final tree) left.
   * You can also see the Wikipedia article
   *  https://en.wikipedia.org/wiki/Huffman_coding
   */
  val groupedBytes = bytes.groupBy(identity)
  val freqMapView = groupedBytes.view.mapValues(_.size)
  val freqMap = freqMapView.toMap

  val orderingByFrequency = Ordering.by[Node, Int](_.freq).reverse
  val pq = scala.collection.mutable.PriorityQueue.empty[Node](orderingByFrequency)
  for ((byte, freq) <- freqMap) {
    pq.enqueue(Leaf(byte, freq))
  }
  while (pq.size > 1) {
    val left = pq.dequeue()
    val right = pq.dequeue()

    val parent = Internal(left, right)
    pq.enqueue(parent)
  }
  pq.dequeue()
end buildTree
/**
 * Get the encoding table from a Huffman tree,
 * associating each byte occurring in the tree to its bit encoding.
 * The encoding value of a byte in a leaf node is formed
 * by the path from the root of the tree to the leaf node:
 * - going to the left child adds a 0 (i.e., false),
 * - going to the right adds a 1 (i.e., true).
 */
def getTable(tree: Node): Map[Byte, Vector[Boolean]] = 
  def traverse(node: Node, code: Vector[Boolean]): Map[Byte, Vector[Boolean]] = node match {
    case leaf: Leaf => Map(leaf.symbol -> code)
    case internal: Internal =>
      traverse(internal.left, code :+ false) ++ traverse(internal.right, code :+ true)
  }

  traverse(tree, Vector())
end getTable

/**
 * Encode a Huffman tree in a sequence of bits:
 * - The encoding of an internal node consists of a 0 (i.e., false)
 *   followed by the encoding of the left subtree and
 *    the encoding of the right subtree
 * - The encoding of a leaf node consists of a 1 (i.e., true)
 *   followed by the 8 bits encoding the byte of the leaf
 *   node in the most-significant-bit-first order.
 *   Note: the frequency is not included in the encoding,
 *   it is not needed when decoding a byte sequence based on the tree.
 */
def encodeTree(tree: Node): Vector[Boolean] =
  ???
end encodeTree


/**
 * The reverse of encodeTree.
 * The freq-values of the leaf nodes shall be 0 in the decoded tree.
 */
def decodeTree(bits: Vector[Boolean]): Node =
  ???
end decodeTree
