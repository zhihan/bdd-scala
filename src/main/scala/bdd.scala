/*
 This scala library is written based on the ML library for BDD written by 
 Jean-Christophe Filliatre. 

 There are internal BDD packages written by MathWorks employee but this one
 is much simplier in its style.

*/

package my.bdd

import scala.math._ // abs, min, mod
import scala.collection.mutable.ArrayBuffer // array

abstract class View 

case class Variable(i: Int)

case class Bdd(val tag: Int, val node: View)
{
  def view = node
}

case object Zero extends View

case object One extends View

case class Node(val v: Variable, val lo: Bdd, val hi:Bdd) extends View
{
}

object Hash {
  def hash_node(lo:Bdd, hi:Bdd) = abs(19 * lo.tag + hi.tag)
  def apply(v:View) = { 
    v match {
      case Zero => 0
      case One => 1
      case Node(_, lo, hi) => hash_node(lo, hi)
    }
  }
} 

object Gentag {
  var r = -1
  def apply() = {
    r +=1
    r
  }
}

class Table {
  // The original implementation in OCaml uses array of weak pointers
  // that are dynamically reallocated. 
  // Here we use array buffer for simplicity. 
  val table = ArrayBuffer[ArrayBuffer[Bdd]]()

  // Iterate over all entries in the 2-D table
  def iter(f: Bdd=>Unit, t: Table) {
    table.foreach{ row =>
      row.foreach { v => f(v) }
    }
  }

  def count() = {
    var i = 0
    table.foreach { row =>
      i += row.length
                 }
    i
  }

  private def addNodeAtRow(d:Bdd, index:Int) {
    // Insert the bdd node at the corresponding index in the
    // table. 
    table(index).append(d)
  }

  def addNode(d:Bdd) {
    // Compute hash code and use it modulo table length
    // as the index of the entry
    addNodeAtRow(d, Hash(d.node) % table.length)
  }
  
  def hashConsNode(v: Variable, l: Bdd, h: Bdd) = {
    val index = Hash.hash_node(l,h) % table.length
    val bucket = table(index)
    val d = bucket.find( _ match {
      case Bdd(_,Node(va, lo, hi)) =>
	(va==v) &&(lo == l) && (hi == h)
    })
    if (d.isEmpty) {
      val newNode = Bdd(Gentag(), Node(v, lo=l, hi=h))
      bucket.append(newNode)
      newNode
    } else {
      // Return the entry in the hash table
      d.get
    }
  }

}

object Table {
  // Factory method for table
  def apply(size:Int) = {
    val sz = if (size<7) 7 else size
    val table = new Table()

    for (i <- 1 to sz) {
      table.table +=  ArrayBuffer[Bdd]()
    }

    table
  }
}

object Utils {
  def nextSz(n:Int) = 3*n/2 +3
  

}
