package basic.datastructure

trait TreeSet[+A] {
  
  def addEl[T >: A](v: T)(implicit ord: Ordering[T]): TreeSet[T] = this match {
    case EmptySet => BranchSet(v,EmptySet,EmptySet)
    case BranchSet(e,l,r) =>  {
      if(e == v) this
      else if(ord.lt(v, e)) BranchSet(e,l.addEl(v),r)
      else BranchSet(e,l,r.addEl(v))
    }
  }
  
  def fold[B](z: B)(f: A => B)(g: (B,B,B) => B): B = this match {
    case EmptySet => z
    case BranchSet(e,l,r) => g(f(e),l.fold(z)(f)(g),r.fold(z)(f)(g))
  }
  
  def union[T >: A](t: TreeSet[T])(implicit ord: Ordering[T]): TreeSet[T] = this match {
    case EmptySet => t
    case BranchSet(e,l,r) => l union (r union (t.addEl(e))) 
  }
  
  def filter[T >: A](p: T => Boolean)(implicit ord: Ordering[T]): TreeSet[T] = 
    fold(EmptySet:TreeSet[T])(x => if(p(x)) BranchSet(x,EmptySet,EmptySet) else EmptySet)((a,b,c) => a union (b union c) )
    
  def contains[T >: A](t: T)(implicit ord: Ordering[T]): Boolean = this match {
    case EmptySet => false
    case BranchSet(e,l,r) => 
      if(e == t) true else if(ord.lt(e, t)) r contains t else l contains t 
  }
  
  def remove[T >: A](t: T)(implicit ord: Ordering[T]): TreeSet[T] =
    fold(EmptySet: TreeSet[T])( a => if(a == t) EmptySet else BranchSet(a,EmptySet,EmptySet))((a,b,c) => a union (b union c))
}

case object EmptySet extends TreeSet[Nothing] 
case class BranchSet[+A](e: A, l: TreeSet[A], r: TreeSet[A]) extends TreeSet[A] 

object TreeSet {
  def apply[A](xs: A *)(implicit ord:Ordering[A]): TreeSet[A] = {
    if(xs.isEmpty) EmptySet
    else {
      def go(ys: A *)(t:TreeSet[A]): TreeSet[A] = {
        if(ys.isEmpty) t
        else go(ys.tail: _ *)(t.addEl(ys.head))
      }
      go(xs:_ *)(EmptySet)
    }
  }
  
}

object TreeSetDriver extends App {
    val xs = TreeSet(1,10,2,9,3,8,4,7,5,6)
    println(xs)
    
    val ys = TreeSet(6,7)
    
    println(xs union ys)
    
    println(xs filter(x => x > 2))
    
    println(xs contains 4)
    println(xs remove 4)
}