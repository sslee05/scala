package basic.datastructure

object MyTree {
  
  /**
   * leaf 과 branch 갯수 구하기 
   */
  def size[A](t:MyTree[A]):Int = {
    t match {
      case MyLeaf(_) => 1
      case MyBranch(l,r) => 1 + size(l) + size(r)
    }
  }
  
  /**
   * node 에서 가자 큰 값 구하기 
   */
  def maximum(t:MyTree[Int]):Int = {
    t match {
      case MyLeaf(n) => n
      case MyBranch(l,r) => maximum(l) max maximum(r)
    }
  }
  
  /**
   * 가장 긴 depth 길이 구하
   */
  def depth[A](t:MyTree[A]):Int = {
    t match {
      case MyLeaf(_) => 1
      case MyBranch(l,r) => 1 + (depth(l) max depth(r))
    }
  }
  
  /**
   * map 적용 하기 
   */
  def map[A,B](t:MyTree[A])(f:A => B):MyTree[B] = {
    t match {
      case MyLeaf(n) => MyLeaf(f(n))
      case MyBranch(l,r) => MyBranch(map(l)(f),map(r)(f))
    }
  }
  
  /**
   * fold 연산 만들기 
   */
  def fold[A,B](t:MyTree[A])(f:A => B)(g:(B,B) => B):B = {
    t match {
      case MyLeaf(n) => f(n)
      case MyBranch(l,r) => g(fold(l)(f)(g),fold(r)(f)(g))
    }
  }
  
  /**
   * fold로 size 구현하기 
   */
  def sizeViaFold[A](t:MyTree[A]):Int = {
    fold(t)(a => 1)( (c1,c2) => 1 + c1 + c2)
  }
  
  /**
   * fold 로 maximum 구현하기 
   */
  def maximumViaFold(t:MyTree[Int]):Int = {
    fold(t)(a => a)((c1,c2) => c1 max c2)
  }
  
  /**
   * fold 로 depth 구현하기 
   */
  def depthViaFold[A](t:MyTree[A]):Int = {
    fold(t)(a => 1)((c1,c2) => 1 + (c1 max c2))
  }
  
  /**
   * fold 로 map 구현하기 
   */
  def mapViaFold[A,B](t:MyTree[A])(f:A => B):MyTree[B] = {
    fold(t)(a => MyLeaf(f(a)):MyTree[B])((c1,c2) => MyBranch(c1,c2))
  }
  
}

sealed trait MyTree[+A]
sealed case class MyLeaf[+A](n:A) extends MyTree[A]
sealed case class MyBranch[+A](l:MyTree[A],r:MyTree[A]) extends MyTree[A]