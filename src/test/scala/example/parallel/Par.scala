package example.parallel

trait Par[+A] {
  def get:A
}

case class MyPar[+A](get: A) extends Par[A]

object Par {
  
  def unit[A](a: A) = MyPar(a)
  def map2[A,B,C](a: Par[A],b: Par[B])(f: (A,B) => C): Par[C] = {
    val va = a.get
    val vb = b.get
    unit(f(va,vb))
  }
  
  def lazyMap2[A,B,C](a: => Par[A],b: => Par[B])(f: (A,B) => C): Par[C] = {
    val va = a.get
    val vb = b.get
    unit(f(va,vb))
  }
  
  def sum(xs: IndexedSeq[Int]): Par[Int] = {
    
    if(xs.size <= 1) {
      
      unit(xs.headOption getOrElse 0)
    }
    else {
      val (l,r) = xs.splitAt(xs.length / 2)
      //map2(sum(l),sum(r))((a,b) => a + b)
      lazyMap2(sum(l),sum(r))((a,b) => a + b)
    }
  }
  
}

object ParDriver extends App {
  import example.parallel.Par._
  val xs = IndexedSeq(1,2,3,4)
  println(sum(xs).get)
}