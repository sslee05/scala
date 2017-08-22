package basic.datastructure

trait MyTree[+A]
case class Node[+A](n: A) extends MyTree[A]
case class Branch[+A](l: MyTree[A], r: MyTree[A]) extends MyTree[A]

object MyTree {
  
  def size[A](t: MyTree[A]): Int = t match {
    case Node(x) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }
  
  def maximum[A](t:MyTree[A])(implicit ord: Ordering[A]): A = t match {
    case Node(x) => x
    case Branch(l,r) => {
      val lv = maximum(l)
      val rv = maximum(r)
      if(ord.lt(lv, rv)) rv else lv
    }
  }
  
  def depth[A](t: MyTree[A]): Int = t match {
    case Node(x) => 1
    case Branch(l,r) => depth(l) max depth(r)
  }
  
  def map[A,B](t: MyTree[A])(f: A => B): MyTree[B] = t match {
    case Node(x) => Node(f(x))
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
  }
  
  def fold[A,B](t: MyTree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Node(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g),fold(r)(f)(g)) 
  }
  
  def sizeViaFold[A](t: MyTree[A]): Int = 
    fold(t)(a => 1)( (l,r) => 1 + l + r)
    
  def depthViaFold[A](t: MyTree[A]): Int = 
    fold(t)(a => 1)((l,r) => l max r)
    
  def maxViaFold[A](t: MyTree[A])(implicit ord: Ordering[A]): A = 
    fold(t)(a => a)((l,r) => if(ord.lt(l,r)) r else l)
  
}

object MyTreeDriver extends App {
  import basic.datastructure.MyTree._
  
  val t = Branch(Branch(Node(1),Node(2)),Branch(Branch(Node(4),Node(5)),Node(6)))
  println(maximum(t))
  println(fold(t)(a => Node(a * 2):MyTree[Int])((l,r) => Branch(l,r)))   
  println(maxViaFold(t))
      
}