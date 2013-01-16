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

case class Node(x: Variable, lo: Bdd, hi:Bdd) extends View
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
  val table = ArrayBuffer[ArrayBuffer[Option[Bdd]]]()
  var totalSize: Int = 0
  var limit: Int = 0
}

object Table {
  // Factory method for table
  def apply(size:Int) = {
    val sz = if (size<7) 7 else size
    val emptyBucket = ArrayBuffer[Bdd]()
    val table = new Table()

    // Note that emptyBucket is shared among the table rows
    for (i <- 1 to sz) {
      table.table +=  emptyBucket
    }

    table.totalSize = sz
    table.limit = 3
    table
  }
}

object Utils {
  // Iterate over all entries in the 2-D table
  def iter(f: Bdd=>Unit, t: Table) {
    t.table.foreach{ row =>
      row.foreach { v =>
        v match {
          case Some(x) => f(v)
          case None => ()
        }
      }
    }
  }

  def count(t:Table) = {
    var i = 0
    t.table.foreach { row =>
      row.foreach { v =>
        v match {
          case Some(x) => i+=1
          case None => ()
        }
                 }
                   }
    i
  }

  def nextSz(n:Int) = 3*n/2 +3
  
  def add_entry_at_index(t:Table, d:Bdd, index:Int) {
    // Insert the bdd node at the corresponding entry in the
    // table. 
  }
  def add_entry(t:Table, d:Bdd) {
    // Compute hash code and use it modulo table length
    // as the index of the entry
    add_entry_at_index(t, d, Hash(d.node) % t.table.length)
  }
  
  
}
