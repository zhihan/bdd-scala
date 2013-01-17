/*
 This scala library is written based on the ML library for BDD written by 
 Jean-Christophe Filliatre. 

 There are internal BDD packages written by MathWorks employee but this one
 is much simplier in its style.

*/

package my.bdd

import scala.math._ // abs, min, mod, pow
import scala.util.Random
import scala.collection.mutable.ArrayBuffer // array
import scala.collection.mutable.HashMap 
import scala.collection.mutable.HashSet

abstract class View 

case class Variable(id: Int)
{
  if (id<0) throw new RuntimeException("Invalid id")
}

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
  // Initialize 
  var r = 1
  def apply() = {
    r +=1
    r
  }
  def reset() = {
    // Don't reset gentag to -1
    // As 0 and 1 are reserved for concrete values
    r = 1
  }
}

class Table(size:Int) {
  // The original implementation in OCaml uses array of weak pointers
  // that are dynamically reallocated. 
  // Here we use array buffer for simplicity. 
  var table = createTable(size)
  var limit = 3

  // Iterate over all entries in the 2-D table
  private def iter(tab:ArrayBuffer[ArrayBuffer[Bdd]], f: Bdd=>Unit) {
    tab.foreach{ row =>
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

  def createTable(size:Int) = {
    val sz = if (size<7) 7 else size
    val t = ArrayBuffer[ArrayBuffer[Bdd]]()
    
    for (i <- 1 to sz) {
      t.append(ArrayBuffer[Bdd]())
    }
    t
  }

  private def nextSize(n:Int) = (3*n/2 + 3)

  def resize() {
    // Resize table prevents a hash bucket grows too big

    val newSize = nextSize(table.length)
    // Initialize
    val newTable = createTable(newSize)
    val oldTable = table

    table = newTable
    iter(oldTable, (x:Bdd) => {
      addNode(x)
    } )
    
    limit += 2 // increase the limit
  }
  
  private def addNodeAtRow(d:Bdd, index:Int) {
    // Insert the bdd node at the corresponding index in the
    // table. 
    table(index).append(d)
    
    if (table(index).length > table.length * limit) {
      // bucket is significantly longer than table
      resize()
    }
  }

  def addNode(d:Bdd) {
    // Compute hash code and use it modulo table length
    // as the index of the entry
    addNodeAtRow(d, Hash(d.node) % table.length)
  }
  
  def hashConsNode(v:Variable, l: Bdd, h: Bdd) = {
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

  def stat() = {
    "Table size:" + table.length + "; " +
    "count:" + count() + "; " +
    "limit:" + limit
  }

}

object Table {
  // Factory method for table
  def apply(size:Int) = {
    new Table(size)
  }
}

object VarTables {
  var maxVar = 0
  val varTable = ArrayBuffer[Table]()

  // setMaxVar resets the cache
  def setMaxVar(n: Int, m:Int) {
    maxVar = n
    varTable.clear()
    for (i <- 1 to n) {
      varTable.append(Table(m))
    }
    Gentag.reset
    Factory.reset
  }
  
  def apply(v: Variable) = varTable(v.id)
}

object Util {
  
  def getId(d:Bdd) = {
    d.node match {
      case Zero | One => VarTables.maxVar 
      case Node(v,_,_) => v.id
    }
  }

  def getVar(d:Bdd) = {
    d.node match {
      case Zero | One => throw new RuntimeException("Wrong var")
      case Node(v,_,_) => v
    }
  }
  
  def low(d:Bdd) = {
    d.node match {
      case Zero | One => throw new RuntimeException("Wrong low")
      case Node(_,l,_) => l
    }
  }

  def high(d:Bdd) = {
    d.node match {
      case Zero | One => throw new RuntimeException("Wrong low")
      case Node(_,_,h) => h
    }
  }

  def writeGraphviz(root:Bdd):String = {
    // Here we only use the tag as the sole Id to identify
    // a Bdd node
    val visited = HashSet[Int]()
    var s = ""
    def visit(v: Bdd) {
      if (!visited.contains(v.tag)) {
        v.node match {
          case Zero => s += (v.tag + "[shape=\"box\"][label=\"0\"]\n")
          case One => s += (v.tag + "[shape=\"box\"][label=\"1\"]\n")
          case Node(Variable(i), l, h) => {
            s += (v.tag + "[shape=\"ellipse\"][label=\"x" + i + "\"]\n")
            s += v.tag + " -> " + h.tag + "\n"
            s += v.tag + " -> " + l.tag + "[style=\"dashed\"]"  + "\n"
            visit(h)
            visit(l)
          }
        }

        visited += v.tag
      }
    }
    visit(root)
    "digraph G {\n" + s + "}\n" 
  }
}

sealed abstract class LogicalOperator
case object And extends LogicalOperator
case object Or extends LogicalOperator

object Eval {
  def concrete(op:LogicalOperator, a:Boolean, b:Boolean) = {
    op match {
      case And => a && b
      case Or => a || b
    }
  }

  def symbolic(op:LogicalOperator, a:Boolean, b:Bdd):Bdd = {
    op match {
      case And => {
        if (!a) Factory.zero else b
      }
      case Or => {
        if (a) Factory.one else b
      }
    }
  }

  def symbolic(op:LogicalOperator, a:Bdd, b:Boolean) = {
    op match {
      case And => if (!b) Factory.zero else a
      case Or => if (b) Factory.one else a
    }
  }

}

final class BddIdentity(val me: Bdd) {
  // Original OCaml implementation defines a struct for identifying
  // BDD identities. This implementation uses a wrapper object.

  // This object uses custom hash code for memorization. 
  override def equals(other: Any) = {
    other match {
      case that: BddIdentity => (me == that.me)
        case _ => false
    }
  }
  
  override def hashCode: Int = {
    me.tag
  }
}

object Factory{
  val zero = Bdd(0, Zero)
  val one = Bdd(1, One)

  def fromBool(b: Boolean) = {
    if (b) one else zero
  }

  def fromSymbol(x: Bdd) = {
    if (x == zero) {
      false
    } else if (x == one) {
      true
    } else {
      throw new RuntimeException("No concrete value")
    }
  }


  def hasConcreteValue(x:Bdd) = {
    (x == zero) || (x == one)
  }

  def mk(v:Variable, l:Bdd, h:Bdd) = {
    if (l == h) {
      l
    } else {
      val table = VarTables(v)
      table.hashConsNode(v, l, h)
    }
  }

  def mkVar(v:Variable) = {
    if (v.id >=  VarTables.maxVar) throw new RuntimeException("Invalid id")
    mk(v, zero, one)
  }


  object NotFactory {
    // Factory a Bdd from not expression
    val cache = HashMap[BddIdentity, Bdd]()
    def clearCache {
      cache.clear()
    }

    private def apprec(x:Bdd): Bdd = {
      val identity = new BddIdentity(x)
      if (cache.contains(identity)) {
        cache(identity)
      } else {
        val res = x.node match {
          case Zero => one
          case One => zero
          case Node(v, l, h) => mk(v, apprec(l), apprec(h))
        }
        cache(identity) = res
        res
      }
    }
    def apply(x:Bdd): Bdd = {
      clearCache
      apprec(x)
    }
  }
  
  def mkNot(x:Bdd) = NotFactory(x)

  final class BddPair(val lo: Bdd, val hi:Bdd) {
    // A tag-based custom-hashed object for memoization. 
    override def equals(other: Any) = {
      other match {
        case that: BddPair => (lo == that.lo) && (hi == that.hi)
        case _ => false
      }
    }

    override def hashCode: Int = {
      val s = lo.tag + hi.tag
      abs(s*(s+1)/2 + hi.tag)
    }
  }

  class OpFactory(val op:LogicalOperator) {
    // Factory a Bdd from logical operators
    val cache = HashMap[BddPair, Bdd]()

    def clearCache {
      cache.clear()
    }

    def apply(a:Bdd, b:Bdd) = {
      clearCache
      apprec(a,b)
    }

    private def apprec(a:Bdd, b:Bdd):Bdd = {
      if (hasConcreteValue(a)) {
        Eval.symbolic(op, fromSymbol(a), b)
      } else if(hasConcreteValue(b)) {
        Eval.symbolic(op, a, fromSymbol(b))
      } else {
        // A and b are symbolic Bdds
        val identity = new BddPair(a,b)
        if (cache.contains(identity)) {
          cache(identity)
        } else {
          val aId = Util.getId(a)
          val bId = Util.getId(b)
          val aVar = Util.getVar(a)
          val bVar = Util.getVar(b)
          val res = if (aId == bId) {
            mk(aVar, 
               apprec(Util.low(a), Util.low(b)), 
               apprec(Util.high(a), Util.high(b)))
          } else if (aId < bId) {
            mk(aVar, 
               apprec(Util.low(a), b),
               apprec(Util.high(a), b))
          } else {
            // aId > bId
            mk(bVar,
               apprec(a, Util.low(b)),
               apprec(a, Util.high(b)))
          }
          cache(identity) =res
          res
        }
      }
    }
  }
  val andFactory = new OpFactory(And)
  def mkAnd(a:Bdd, b:Bdd) = andFactory(a,b)

  val orFactory = new OpFactory(Or)
  def mkOr(a:Bdd, b:Bdd) = orFactory(a,b)

  def reset() {
    NotFactory.clearCache
    andFactory.clearCache
    orFactory.clearCache
  }
}

object Sat {
  def isSat(b: Bdd) = b.node != Zero
  def isTautology(b: Bdd) = b.node == One

  def countSat(bb:Bdd) = {
    val cache = HashMap[BddIdentity,BigInt]()
    def count(b:Bdd):BigInt = {
      val identity = new BddIdentity(b)
      if (cache.contains(identity)) {
	cache(identity)
      } else {
	val n = b.node match {
	  case Zero => BigInt(0)
	  case One => BigInt(1)
	  case Node(v,l,h) => {
	    val dvl = Util.getId(l) - v.id -1
	    val dvh = Util.getId(h) - v.id -1
	    BigInt(2).pow(dvl) * count(l) + BigInt(2).pow(dvh)*count(h)
	  }
	}
	cache(identity) = n
	n
      }
    }
    count(bb)
  } 

  def randomSat(b:Bdd) = {
    val res = ArrayBuffer[(Int,Boolean)]()
    val ran = new Random()
    def walk(x:Bdd) {
      x.node match {
	case Zero => throw new RuntimeException("not found")
	case One => Unit
	case Node(v, Bdd(_,Zero), h) => {
	  res.append((v.id, true))
	  walk(h)
	}
	case Node(v, l, Bdd(_,Zero)) => {
	  res.append((v.id, false))
	  walk(l)
	}
	case Node(v, l, h) => {
	  if (ran.nextBoolean()) {
	    res.append((v.id, false))
	    walk(l)
	  } else {
	    res.append((v.id, true))
	    walk(h)
	  }
	}
      }
    }
    walk(b)
    res.toList
  }
    

}
