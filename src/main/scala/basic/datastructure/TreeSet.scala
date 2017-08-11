package basic.datastructure

trait TreeSet {
  
  def addEl(v: Int): TreeSet = this match {
    case EmptySet => BranchSet(v,EmptySet,EmptySet)
    case BranchSet(e,l,r) =>  {
      if(e == v) this
      else if(e > v) BranchSet(e,l.addEl(v),r)
      else BranchSet(e,l,r.addEl(v))
    }
  }
  
  def fold[B](z: B)(f: Int => B)(g: (B,B,B) => B): B = this match {
    case EmptySet => z
    case BranchSet(e,l,r) => g(f(e),l.fold(z)(f)(g),r.fold(z)(f)(g))
  }
  
  def union(t: TreeSet): TreeSet = this match {
    case EmptySet => t
    case BranchSet(e,l,r) => l union (r union (t addEl e)) 
  }
  
  def filter(p: Int => Boolean): TreeSet = 
    fold(EmptySet:TreeSet)(x => if(p(x)) BranchSet(x,EmptySet,EmptySet) else EmptySet)((a,b,c) => a union (b union c) )
    
}

case object EmptySet extends TreeSet 
case class BranchSet(e: Int, l: TreeSet, r: TreeSet) extends TreeSet 

object TreeSet {
  def apply(xs: Int*): TreeSet = {
    if(xs.isEmpty) EmptySet
    else {
      def go(ys: Int *)(t:TreeSet): TreeSet = {
        if(ys.isEmpty) t
        else go(ys.tail: _ *)(t.addEl(ys.head))
      }
      go(xs:_ *)(EmptySet)
    }
  }
}

object TreeSetDriver extends App {
    val xs = TreeSet(1,2,3,4,5)
    println(xs)
    
    val ys = TreeSet(6,7)
    
    println(xs union ys)
    
    println(xs filter(x => x > 2))
}