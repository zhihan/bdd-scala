package me.zhihan.bdd

object StringOp {
  def join(a:String, b:String) = a + "-" + b

  def split(a:String) = {
    val i = a.indexOf("-")
    val x = a.substring(0, i)
    val y = a.substring(i+1, a.length())
    (x,y)
  }
}

