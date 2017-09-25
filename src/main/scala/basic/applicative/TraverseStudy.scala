package basic.applicative

object TraverseStudy {
  //traverse 와 sequence 는 flatMap에 직접 위존 적이지 않았다.
  /*
  def traverse[A,B](xs: List[A])(f: A => F[B]): F[List[B]] = 
      xs.foldRight[F[List[B]]](unit(List()))((a,b) => map2(f(a),b)((a1,b1) => a1::b1))
  
  def sequence[A](xs: List[F[A]]): F[List[A]] = 
      xs.foldRight[F[List[A]]](unit(List()))((a,b) => map2(a,b)((a1,b1) => a1::b1))
  */

  //우선 applicative functor를 보자.
  trait Functor[F[_]] {
    def map[A, B](a: F[A])(f: A => B): F[B]
  }

  trait Applicative[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      apply(map(ma)(f.curried))(mb)

    def apply[A, B](fab: F[A => B])(ma: F[A]): F[B] =
      map2(fab, ma)((f, a) => f(a))

    //ex-01) applicative trait에 sequenceList를 구현하라.
    def sequenceList[A](xs: List[F[A]]): F[List[A]] =
      xs.foldRight[F[List[A]]](unit(List()))((a, b) => map2(a, b)((a1, b1) => a1 :: b1))

    //ex-03) applicative trait에 sequenceMap를 구현하라.
    def sequenceMap[K, V](xs: Map[K, F[V]]): F[Map[K, V]] =
      xs.foldLeft[F[Map[K, V]]](unit(Map.empty)) {
        case (b, (k, v)) => map2(b, v)((b1, v1) => b1 + (k -> v1))
      }

    def map[A, B](ma: F[A])(f: A => B): F[B] =
      apply(unit(f))(ma)
      
    //ex-16) 다른 Applicative Functor를 곱하는 product 함수를 구현하라.  
    def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x],G[x])})#f] = {
      val self = this
      new Applicative[({ type f[x] = (F[x],G[x])})#f] {
        def unit[A](a: => A): (F[A],G[A]) = (self.unit(a),G.unit(a))
        override def apply[A,B](fab: (F[A => B],G[A => B]))(p: (F[A],G[A])) = 
          (self.apply(fab._1)(p._1),G.apply(fab._2)(p._2) )
      }
    }

  }

  //ex-04) 이 순회 구조를 일반화 하자.
  trait Traverse[F[_]] extends Functor[F] { self =>
    def traverse[G[_]: Applicative, A, B](xs: F[A])(f: A => G[B]): G[F[B]]
    def sequence[G[_]: Applicative, A](xs: F[G[A]]): G[F[A]] =
      traverse(xs)(a => a)

    //ex-08) Functor인 map function을 traverse로 구현하라.
    /*
    B => B 를 만들어 주는 unit 함수가 필요한다.
    따라서 가장 단순한 monoid인 Id 를 만들면 된다.
     */
    //ex-08-01) A 를 받으면 A를 돌려주는 Id monad를 생성하라.
    type Id[A] = A

    val idMonad = new Monad[Id] {
      def unit[A](a: => A) = a
      override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma)
    }

    //ex-08-02) idMonad를 이용하여 Functor인 map function을 traverse로 구현하라
    //
    def map[A, B](ma: F[A])(f: A => B): F[B] =
      traverse[Id, A, B](ma)(f)(idMonad)

    import basic.state.State
    import basic.state.State._
    def stateMonad[S] = new Monad[({ type StateS[A] = State[S, A] })#StateS] {
      def unit[A](a: => A): State[S, A] = State.unit(a)
      override def flatMap[A, B](s: State[S, A])(f: A => State[S, B]): State[S, B] =
        s flatMap f
    }

    //ex-10) State 동작과 traverse 를 이용한 내부상태를 유지하면서 자료구조를 훑는 다음의 함수를 작성하라.
    def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
      traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

    //ex-11)traverseS 함수를 이용해서 0부터 시작하여 1씩증가하는 내부상태 를 구하는 다음의 함수를 구하라.
    def zipWithIndex_[A](ta: F[A]): F[(A, Int)] =
      traverseS(ta)((a: A) => (for {
        i <- get[Int]
        _ <- set(i + 1)
      } yield (a, i))).run(0)._1

    //ex-12)임이의 Traverse Functor를 목록으로 반한하는 다음의 함수를 구하라.
    def toList_[A](ta: F[A]): List[A] =
      traverseS(ta)((a: A) => (for {
        i <- get[List[A]]
        _ <- set(a :: i)
      } yield ())).run(Nil)._2.reverse

    //State 를 이용한 순회는 
    //현재상태을 얻고 -> 다음상태를 계산하고 -> 그것을 현재상태로 갱신하고 -> 어떤 값을 산출(yield)
    //하는 패턴으로 일반화 가능하다.
    //ex-13) 이를 구현하라.
    def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
      traverseS(fa)((a: A) => (for {
        s1 <- get[S]
        (b, s2) = f(a, s1)
        _ <- set(s2)
      } yield (b))).run(s)

    //ex-14)mapAccum으로 zipWithIndex를 다시 구현하라.
    def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
      mapAccum(ta, 0)((a, s) => ((a, s), s + 1))._1

    //ex-15) mapAccum으로 toList를 다시 구현하라.
    def toList[A](fa: F[A]): List[A] =
      mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse
      
//    def fuse[M[_],N[_],A,B](fa: F[A])(f: A => M[B], g: A => N[B])
//                         (implicit M: Applicative[M], N: Applicative[N]): (M[F[B]], N[F[B]]) =
//    traverse[({type f[x] = (M[x], N[x])})#f, A, B](fa)(a => (f(a), g(a)))(M product N)

      
    //순회의 융합 
    //ex-17) applicative functor를 이용하여 두순외의 유합을 산출하는 다음의 함수를 작성하라.
    //두 함수 f 와 g가 주어졌을 때 이함수는 fa를 한 번만 순회해야 한다.
    // Applicative 에 Applicative functor를 곱하는 product function 이 필요한다.
    def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
        (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = 
      traverse[({type f[x] = (G[x],H[x])})#f,A,B](fa)(a => (f(a),g(a)))(G product H)
    
      
//       new Traverse[({type f[x] = F[G[x]]})#f] {
//      override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]) =
//        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
//    }
    
    //ex-18) traverse functor를 합성하는 다음의 함수를 구현하라.
    def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = 
      new Traverse[({type f[x] = F[G[x]]})#f] {
        def traverse[M[_]: Applicative, A, B](fa: F[G[A]])(f: A => M[B]) = 
          self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
      }
  }

  //ex-08-01) 모든 Monad는 Applicative Functor 이므로   

  trait Monad[F[_]] extends Applicative[F] {
    def unit[A](ma: => A): F[A]
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

    override def map[A, B](m: F[A])(f: A => B): F[B] =
      flatMap(m)(a => unit(f(a)))

    override def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))
      
    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  }

  object Monad {
    import basic.state.State
    def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))
      override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }
    
    //ex-19) Monad합성은 힘들다. 편법을 써야 하는데 F[G[F[G[A]]]] 인경우 G에 대한 Traverse 가 있다면 가능하다.
    // Traverse를 이용하여 Monad compose 인 다음 함수를 구현하라.
    def composeM[F[_],G[_]](implicit F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] = 
      new Monad[({type f[x] = F[G[x]]})#f] {
        def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
        override def flatMap[A,B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] = 
          F.flatMap(fga)(ga => F.map(T.traverse(ga)(f))(gga => G.join(gga)))
          //T.traverse(ga)(A => F[G[B]]) -> F[G[G[B]]]
          //G.map(F[G[G[B]]]])(G.join)   -> F[G[B]] 
          //reuslt                       -> F.flatMap(F[G[A]])(ga => F[G[B]]) 
      }
  }
  
  /*
  //ex-20)monad 합성은 힘들기 때문에 monad마다 특별한 monad 합성을 이용한다. 이를 monad 변환기라 한다.
  //Option에 monad에 대한 임이의 Monad M의 합성를 구현하라.
  case class OptionT[M[_],A](value: M[Option[A]])(implicit M: Monad[M]) {
    def flatMap[B](f: A => OptionT[M,B]): OptionT[M,B] = 
      OptionT(value flatMap {
        case None => M.unit(None)
        case Some(a) => f(a).value
      })
  }
	*/
  
  /**
   * type ConstInt[A] = Int 가 있다고 하자
   * 이는 무조건 Int 유형을 반환하는 container 이다.
   * def traverse[G[_]: Applicative, A, B](xs: F[A])(f: A => G[B]): G[F[B]]
   * 이제 위의 traverse의 G[B] 이것을 ConstInt 로 바꾸보자.
   * def traverse[ConstInt,A,B](xs: F[A])(f: A => ConstInt): ConstInt[F[B]]
   * def raverse[A,B](xs: F[A])(f: A => Int): Int
   *
   * Foldable의 foldMap 과 비슷하다.
   * def foldMap[A,B](xs: F[A])(f: A => B)(m: Monoid[B]): B
   *
   * 따라서 foldMap도 traverse로 일반화
   * Traverse 는 Foldable 이다. 하고 말할 수 있다.
   */

  //ex-09) Traverse 를 Foldable이라 할 수 있게 구현하라.
  import basic.monoid.Foldable
  import basic.monoid.Monoid
  type Const[M, B] = M
  trait TraverseV2[F[_]] extends Functor[F] with Foldable[F] {
    def traverse[G[_]: Applicative, A, B](ma: F[A])(f: A => G[B]): G[F[B]]
    def sequence[G[_]: Applicative, A](ma: F[G[A]]): G[F[A]] =
      traverse(ma)(a => a)

    type Id[A] = A
    val idM = new Monad[Id] {
      def unit[A](a: => A): Id[A] = a
      override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma)
    }

    def map[A, B](ma: F[A])(f: A => B): F[B] =
      traverse[Id, A, B](ma)(f)(idM)

    type Const[M, B] = M
    implicit def monoidApplicative[M](m: Monoid[M]) = new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = m.zero
      override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = m.op(m1, m2)
    }

    override def foldMap[A, M](xs: F[A])(f: A => M)(mb: Monoid[M]): M =
      traverse[({ type f[x] = Const[M, x] })#f, A, Nothing](xs)(a => f(a))(monoidApplicative(mb))
    //traverse[({type f[x] = Const[B,x]})#f,A,Nothing](as)(f)(monoidApplicative(mb))
  }

}

import basic.applicative.TraverseStudy._

object Traverse {
   //ex-05) ListTraverse 를 구현하라.
  val listTraverse = new Traverse[List] {
    def traverse[G[_], A, B](xs: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      xs.foldRight[G[List[B]]](G.unit(List()))((a, b) => G.map2(f(a), b)((a1, b1) => a1 :: b1))
  }

  //ex-06) OptionTraverse 를 구현하라.
  val optionTraverse = new Traverse[Option] {
    def traverse[G[_], A, B](o: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = o match {
      case Some(a) => G.map(f(a))(a1 => Some(a1))
      case None    => G.unit(None)
    }
  }

  //ex-07) Tree에 대한 TreeTraverse 를 구현하라.
  case class Tree[+A](head: A, tail: List[Tree[A]])
  val treeTraverse = new Traverse[Tree] {
    def traverse[G[_], A, B](t: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(t.head), listTraverse.traverse(t.tail)(t1 => traverse(t1)(f)))(Tree(_, _))
  }
}

object TraverseDriver extends App {

  import basic.state.State
  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  import basic.state.State
  import basic.applicative.Traverse._
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


