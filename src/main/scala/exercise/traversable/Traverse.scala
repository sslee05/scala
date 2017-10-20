package exercise.traversable

import basic.monad.Functor
import basic.applicative.Applicative
import basic.monad.Monad
import basic.monoid.Foldable
import basic.monoid.Monoid
import basic.state.State


//ex-01) Functor를 extends 한 Traverse functor를 선언하라. 
trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[G[_]: Applicative, A, B](ma: F[A])(f: A => G[B]): G[F[B]]
  def sequence[G[_]: Applicative, A](ma: F[G[A]]): G[F[A]] = ???
    
  //ex-02) IdMonad를 만들어 map을 traverse 와 IdMonad를 이용하여 구현하라.
  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = ???
    def flatMap[A,B](ma: Id[A])(f: A => Id[B]): Id[B] = ??? 
  }
  
  def map[A,B](ma: F[A])(f: A => B): F[B] = ???
    
  //ex-08) Foldable를 상속하고 foldMap를 구현하라.
  type Const[M,B] = M
  implicit def monoidApplicative[M](mi: Monoid[M]): Applicative[({ type f[x] = Const[M,x] })#f] = 
    new Applicative[({ type f[x] = Const[M,x] })#f] {
    
    def unit[A](a: => A): Const[M,A] = ???
    override def apply[A,B](fab: Const[M, A => B])(ma: Const[M,A]): Const[M,B] = ??? 
      
    def map2[A,B,C](ma: Const[M,A], mb: Const[M,B])(f: (A,B) => C): Const[M,C] = ???
  }
  
  override def foldMap[A,M](ma: F[A])(f: A => M)(mi: Monoid[M]): M = ???
    
  //ex-09) State 동작과 traverse 를 이용한 내부상태를 유지하면서 자료구조를 훑는 다음의 함수를 작성하라.
  import basic.monad.Monad._
  def traverseS[S,A,B](fa: F[A])(f: A => State[S,B]): State[S,F[B]] = ???
    
  //ex-10) traverseS를 이용해서 0부터 시작하여 1씩증가하는 내부상태 를 구하는 다음의 함수를 구하라.
  import basic.state.State._
  def zipWithIndex_[A](ta: F[A]): F[(A, Int)] = ???
  
  //ex-11)임이의 Traverse Functor를 목록으로 반한하는 다음의 함수를 구하라.
  def toList_[A](ma: F[A]): List[A] = ???
    
  //ex-12)State 를 이용한 순회는
  //현재상태을 얻고 -> 다음상태를 계산하고 -> 그것을 현재상태로 갱신하고 -> 어떤 값을 산출(yield) 
  //하는 패턴으로 일반화 가능하다. 이를 일반화 하라.
  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = ???
  
  //ex-13)mapAccum으로 zipWithIndex를 다시 구현하라.
  def zipWithIndex[A](ta: F[A]): F[(A,Int)] = ???
  
  //ex-14) mapAccum으로 toList를 다시 구현하라.
  override def toList[A](ma: F[A]): List[A] = ???
    
  //ex-15)traversable Functor의 융합하는 다음을 구현하라.
  //두 함수 f 와 g가 주어졌을 때 이함수는 fa를 한 번만 순회해야 한다. 
  //Applicative 에 Applicative functor를 곱하는 product function 이 필요한다.
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
    (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???
    
  //ex-16) traverse functor를 합성하는 다음의 함수를 구현하라.
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
  
  //ex-17) Monad합성은 힘들다. 편법을 써야 하는데 F[G[F[G[A]]]] 인경우 G에 대한 Traverse 가 있다면 가능하다.
  // Traverse를 이용하여 Monad compose 인 다음 함수를 구현하라.
  //def composeM[F[_],G[_]](implicit F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f]
  def composeM[F[_], G[_]](implicit F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({ type f[x] = F[G[x]] })#f] = ???
}

object Traverse {
  
  //ex-03) sequenceList를 구현하라.
  def sequenceList[F[_]: Applicative, A](xs: List[F[A]])(implicit F: Applicative[F]): F[List[A]] = ???
    
  //ex04) sequenceMap를 구현하라.
  def sequenceMap[F[_]: Applicative, K,V](xs: Map[K,F[V]])(implicit F: Applicative[F]): F[Map[K,V]] = ???
  
  //ex-05) list용 Traverse instance 생성을 구현하라.
  val listTraverse = new Traverse[List] {
    def traverse[G[_], A, B](ma: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = ???
  }
  
  //ex-06) Option용 Traverse instance 생성을 구현하라.
  val optionTraverse = new Traverse[Option] {
    def traverse[G[_],A,B](ma: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = ???
  }
  
  //ex-07) 다음의 Tree에 대한 Traverse instance 생성을 구현하라. 앞의 listTraverse를 이용하라.
  case class Tree[+A](head: A, tail: List[Tree[A]])
  val treeTraverse = new Traverse[Tree] {
    def traverse[G[_],A,B](ma: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = ???
  }
}


object TraverseDriver extends App {
  import basic.state.State
  import exercise.traversable.Traverse._
  
  val xs01 = List(1, 2, 3, 4, 5)
  val rx01 = listTraverse.zipWithIndex_(xs01)
  println(rx01)

  val rx02 = listTraverse.toList_(xs01)
  println(rx02)
  
  
  val list = List(Some(1),Some(2),Some(3))
  val rs09 = list flatMap(a => a map(x => x + 1))
  val rs10 = list flatMap(a => Some(a map(x => x + 1)))
  val rs11 = list flatMap(a => List(a map(x => x + 1)))
  
  println(rs09)
  println(rs10)
  println(rs11)
}