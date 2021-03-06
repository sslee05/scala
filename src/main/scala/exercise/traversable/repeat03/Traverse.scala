package exercise.traversable.repeat03

import basic.monad.Functor
import basic.applicative.Applicative
import basic.monad.Monad
import basic.monoid.Foldable
import basic.monoid.Monoid
import basic.state.State


//ex-01) Functor를 extends 한 Traverse functor를 선언하라. 
trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  
  def traverse[G[_]: Applicative, A, B](ma: F[A])(f: A => G[B]): G[F[B]]
  def sequence[G[_]: Applicative, A](mfg: F[G[A]]): G[F[A]] = 
  traverse(mfg)(fg => fg)
    
  //ex-02) IdMonad를 만들어 map을 traverse 와 IdMonad를 이용하여 구현하라.
  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = a
    def flatMap[A,B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma)  
  }
  
  def map[A,B](ma: F[A])(f: A => B): F[B] = 
    traverse[({type f[x] = Id[x]})#f,A,B](ma)(f)(idMonad)
    
  //ex-08) Foldable를 상속하고 foldMap를 구현하라.
  type Const[M,B] = M
  implicit def monoidApplicative[M](mi: Monoid[M]): Applicative[({type f[x] = Const[M,x]})#f] = 
    new Applicative[({type f[x] = Const[M,x]})#f] {
      def unit[A](a: => A): Const[M,A] =  mi.zero
      override def apply[A,B](fab: Const[M,A => B])(ma: Const[M,A]): Const[M,B] = 
        mi.op(fab,ma)
        
      def map2[A,B,C](ma: Const[M,A], mb: Const[M,B])(f: (A,B) => C): Const[M,C] = 
        apply(apply(unit(f.curried))(ma))(mb)
  }
  
  override def foldMap[A,B](ma: F[A])(f: A => B)(mi: Monoid[B]): B = 
    traverse[({type f[x] = Const[B,x]})#f, A, B](ma)(f)(mi)
    
  //ex-09) State 동작과 traverse 를 이용한 내부상태를 유지하면서 자료구조를 훑는 다음의 함수를 작성하라.
  import basic.monad.Monad._
  def traversS[S,A,B](fa: F[A])(f: A => State[S,B]): State[S,F[B]] = 
    traverse[({type f[x] = State[S,x]})#f, A, B](fa)(f)(stateMonad)
    
  //ex-10) traverseS를 이용해서 0부터 시작하여 1씩증가하는 내부상태 를 구하는 다음의 함수를 구하라.
  import basic.state.State._
  def zipWithIndex_[A](ta: F[A]): F[(A, Int)] =
    traversS(ta)(a => for {
      i <- get[Int]
      _ <- set(i + 1)
    }yield((a,i))).run(0)._1
  
  //ex-11)임이의 Traverse Functor를 목록으로 반한하는 다음의 함수를 구하라.
  def toList_[A](ma: F[A]): List[A] = 
    traversS(ma)(a => for {
      xs <- get[List[A]]
      _  <- set(a :: xs)
    }yield(xs)).run(Nil)._2
    
  //ex-12)State 를 이용한 순회는
  //현재상태을 얻고 -> 다음상태를 계산하고 -> 그것을 현재상태로 갱신하고 -> 어떤 값을 산출(yield) 
  //하는 패턴으로 일반화 가능하다. 이를 일반화 하라.
  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A,S) => (B,S)): (F[B], S) = 
  traversS(fa)(a => for {
    s1 <- get[S]
    val (b1,s2) = f(a,s1)
    _  <- set(s2)
  }yield(b1)).run(s)
  
  //ex-13)mapAccum으로 zipWithIndex를 다시 구현하라.
  def zipWithIndex[A](ta: F[A]): F[(A,Int)] = 
    mapAccum(ta, 0)((a,i) => ((a, i + 1),i+1))._1
  //def zipWithIndex[A](ta: F[A]): F[(A,Int)] = ???
  
  //ex-14) mapAccum으로 toList를 다시 구현하라.
    override def toList[A](ma: F[A]): List[A] = 
      mapAccum(ma, Nil: List[A])((a,xs) => (a, a:: xs))._2
    
  //ex-15)traversable Functor의 융합하는 다음을 구현하라.
  //두 함수 f 와 g가 주어졌을 때 이함수는 fa를 한 번만 순회해야 한다. 
  //Applicative 에 Applicative functor를 곱하는 product function 이 필요한다.
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
    (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
      traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))( G product H)
    
  //ex-16) traverse functor를 합성하는 다음의 함수를 구현하라.
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = 
    new Traverse[({type f[x] = F[G[x]]})#f] {
      def traverse[M[_]: Applicative, A, B](mfg: F[G[A]])(f: A => M[B]): M[F[G[B]]] = 
        self.traverse(mfg)(ga => G.traverse(ga)(f))
  }
  
  //ex-17) Monad합성은 힘들다. 편법을 써야 하는데 F[G[F[G[A]]]] 인경우 G에 대한 Traverse 가 있다면 가능하다.
  // Traverse를 이용하여 Monad compose 인 다음 함수를 구현하라.
  def composeM[F[_], G[_]](implicit F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] = 
    new Monad[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
      def flatMap[A,B](mfg: F[G[A]])(f: A => F[G[B]]): F[G[B]] = 
        F.flatMap(mfg)(ga => F.map(T.traverse(ga)(f))(G.join))
  }
}

object Traverse {
  
  //ex-03) sequenceList를 구현하라.
  def sequenceList[F[_]: Applicative, A](xs: List[F[A]])(implicit F: Applicative[F]): F[List[A]] = 
    xs.foldRight[F[List[A]]](F.unit(Nil))((a,b) => F.map2(a, b)(_::_))
    
  //ex04) sequenceMap를 구현하라.
  def sequenceMap[F[_]: Applicative, K, V](xs: Map[K,F[V]])(implicit F: Applicative[F]): F[Map[K,V]] = 
    xs.foldLeft[F[Map[K,V]]](F.unit(Map.empty))({case (b,(k,v)) => F.map2(b,v)((b1,a) => b1 + (k -> a) ) })
  
  //ex-05) list용 Traverse instance 생성을 구현하라.
  val listTraverse = new Traverse[List] {
    def traverse[F[_], A, B](ma: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] = 
      ma.foldRight[F[List[B]]](F.unit(Nil))((a,b) => F.map2(f(a), b)(_::_))
  }
  
  //ex-06) Option용 Traverse instance 생성을 구현하라.
  val optionTraverse = new Traverse[Option] {
    def traverse[G[_], A, B](ma: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = ma match {
      case None => G.unit(None)
      case Some(a) => G.map(f(a))(b => Some(b))
    }
  }
  
  //ex-07) 다음의 Tree에 대한 Traverse instance 생성을 구현하라. 앞의 listTraverse를 이용하라.
  case class Tree[+A](head: A, tail: List[Tree[A]])
  val treeTraverse = new Traverse[Tree] {
    def traverse[G[_], A, B](ma : Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ma.head), listTraverse.traverse(ma.tail)(t => traverse(t)(f)))(Tree(_,_))
      
  }

}

object TraverseDriver extends App {
  import basic.state.State
  import exercise.traversable.repeat03.Traverse._
  
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