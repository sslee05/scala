package basic.strictness

import basic.strictness.Stream._

object strickness {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val ss:Stream[Int] = Stream(1,2,3,4,5)          //> ss  : basic.strictness.Stream[Int] = Cons(basic.strictness.Stream$$$Lambda$1
                                                  //| 0/1811044090@26ba2a48,basic.strictness.Stream$$$Lambda$11/114132791@64616ca2
                                                  //| )
  ss.take(2).toList                               //> res0: List[Int] = List(1, 2)
  
  def exists[A](xs:Stream[A])(p:A => Boolean):Boolean = {
    xs match {
      case Empty => false
      case Cons(h,t) => p(h()) || exists(t())(p)
    }
  }                                               //> exists: [A](xs: basic.strictness.Stream[A])(p: A => Boolean)Boolean
  
  exists(ss)(x => x > 2)                          //> res1: Boolean = true
  
  
  def foldRight[A,B](xs:Stream[A],z: => B)(f:(A, => B) => B):B = {
    xs match {
      case Empty => z
      case Cons(h,t) => f(h(),foldRight(t(),z)(f))
    }
  }                                               //> foldRight: [A, B](xs: basic.strictness.Stream[A], z: => B)(f: (A, => B) => B
                                                  //| )B
  
  def existViaFold[A](xs:Stream[A])(p:A => Boolean):Boolean =
    foldRight(xs,false)((a,g) => p(a) || g)       //> existViaFold: [A](xs: basic.strictness.Stream[A])(p: A => Boolean)Boolean
    
  
  
  def mayBeTwice(b: Boolean, i: => Int): Int = if (b) i + i else 1
                                                  //> mayBeTwice: (b: Boolean, i: => Int)Int

  def mayBeTwiceCache(b: Boolean, i: => Int): Int = {
    lazy val j = i
    if (b) j + j else 1
  }                                               //> mayBeTwiceCache: (b: Boolean, i: => Int)Int

  println(mayBeTwice(true, { println("called"); 2 }))
                                                  //> called
                                                  //| called
                                                  //| 4
  println(mayBeTwiceCache(true, { println("called"); 3 }))
                                                  //> called
                                                  //| 6
  
  
}