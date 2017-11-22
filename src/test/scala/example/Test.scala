package example

object Test extends App {
  
  val xs = List(1,2,3,4,5)
  val rs01 = xs.aggregate(2)((b,a) => {
    println(s"b:$b a:$a")
    b + a
  }, (b1,b2) => {
    println(s"b1:$b1 b2:$b2")
    b1 + b2
  })
  
  println(s"@@@@@@@@@@@@@@=>$rs01")
  println("#########")
    
  val rs = xs.par.aggregate(2)((b,a) => {
    println(s"b:$b a:$a")
    b + a
  }, (b1,b2) => {
    println(s"b1:$b1 b2:$b2")
    b1 + b2
  })
  
  println(s"@@@@@@@@@@@@@@=>$rs")
  
  def reduceLeft[A,B](rs: Seq[B], xs: Seq[A])(f: A => B): Seq[B] = xs match {
    case h +: tail => reduceLeft(f(h) +: rs, tail)(f)
    case Nil => rs
  }
  
  def reduceRight[A,B](rs: Seq[B], xs: Seq[A])(f: A => B): Seq[B] = xs match {
    case h +: tail => f(h) +: reduceRight(rs,tail)(f)
    case Nil => rs
  }
  
  val xs01 = List("a","b","c")
  println(reduceLeft(Nil,xs01)(a => ":"+a))
  println(reduceRight(Nil,xs01)(a => ":"+a))
  
}