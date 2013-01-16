package test.bdd

import my.bdd._

import org.scalatest.FunSuite 


class Suite extends FunSuite {
  test("Constant Not") {
    VarTables.setMaxVar(10,10)
    
    val one = Factory.mkNot(Factory.zero)
    assert( one == Factory.one)

    val zero = Factory.mkNot(Factory.one)
    assert( zero == Factory.zero)

  }

  test("Variable Not") {
    VarTables.setMaxVar(10,10)

    val x = Factory.mkVar(Variable(0))
    val xNot = Factory.mkNot(x)

    val x2 = Factory.mkNot(xNot)
    assert( x == x2 )

  }

  test("Logical operators") {
    VarTables.setMaxVar(10,10)

    val x = Factory.mkVar(Variable(0))
    val xNot = Factory.mkNot(x)

    val x2 = Factory.mkAnd(x, xNot)
    assert(x2 == Factory.zero)

    val x3 = Factory.mkOr(x, xNot)
    assert(x3 == Factory.one)

  }

  test("Regular bdds") {
    VarTables.setMaxVar(10,10)
    val x1 = Factory.mkVar(Variable(0))
    val x2 = Factory.mkVar(Variable(1))
    val f = Factory.mkAnd(x1, x2)
    val v = Factory.mkOr(x1, x2)
    val f2 = Factory.mkAnd(f,v)
    assert(f2 == f)
    val v2 = Factory.mkOr(f,v)
    assert(v2 == v)
    
    // println("x0 and x1")
    // println(Util.writeGraphviz(f))
    
  }
}
