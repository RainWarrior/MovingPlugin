package test

import movingplugin.{ Gather, Inject }

@Gather(Test2) object Test1 {
  println("hello" + this.getClass.getName)
}

@Inject object Test2 {
  println("world" + this.getClass.getName)
}

object Test {
  def main(args: Array[String]) {
    println(1)
    Test2
    println(2)
  }
}
