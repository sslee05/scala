package basic.applicative
import basic.monad.Monad._
import basic.monad.Functor

object ApplicativeStudy {

  

  //ex-01) applicative trait를 선언하라
  trait Functor2[F[_]] {
    def map[A, B](a: F[A])(f: A => B): F[B]
  }
  
  //################ Applicative ############################
  trait Applicative[F[_]] extends Functor2[F] {
    def unit[A](a: => A): F[A]
    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C]

    //ex-02) map를 map2와 unit으로 구현하라.
    def map[A, B](m: F[A])(f: A => B): F[B] =
      map2(m, unit(()))((a, _) => f(a))

    //ex-03) Applicative trait에 traverse 함수를 구현하라.
    def traverse[A, B](xs: List[A])(f: A => F[B]): F[List[B]] =
      xs.foldRight[F[List[B]]](unit(Nil))((a, b) => map2(f(a), b)((a1, b1) => a1 :: b1))

    //ex-04) Applicative trait에 traverse를 이용하여  sequence 함수를 구현하라.
    def sequenceByTraverse[A](xs: List[F[A]]): F[List[A]] =
      traverse(xs)(a => a)

    //ex-05) Applicative trait에 map2 와 unit 으로 sequence 함수를 구현하라.
    def sequence[A](xs: List[F[A]]): F[List[A]] =
      xs.foldRight[F[List[A]]](unit(Nil))((a, g) => map2(a, g)((a1, b1) => a1 :: b1))

    //ex-06) Applicative trait에 map2 와 unit 으로 replicateM 함수를 구현하라.
    def replicateM[A](n: Int, m: F[A]): F[List[A]] =
      if (n <= 0) unit(List[A]()) else map2(m, replicateM(n - 1, m))((a, b) => a :: b)

    //ex-07) Applicative trait에 sequence 으로 replicateM 함수를 구현하라.
    def replicate[A](n: Int, m: F[A]): F[List[A]] =
      sequence(List.fill(n)(m))

    //ex-08) Applicative trait에  map2로 product 함수를 구현하라.
    def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
      map2(ma, mb)((a, b) => (a, b))
      
    //ex-17) 쌍들의 묶음을 바꾸는 다음의 함수를 구현하라.
    def assoc[A,B,C](p: (A,(B,C))): ((A,B),C) = p match {
      case (a,(b,c)) => ((a,b),c)
    }
    
    //ex-18)각각의 Input => Output 2개를 받아 Input 쌍을 받고 Outout 쌍을 받는 함수를 구현하라.
    def productF[I1,I2,O1,O2](f: I1 => O1, g: I2 => O2): (I1,I2) => (O1,O2) = 
      (i1,i2) => (f(i1),g(i2))
    
    //ex-23) Applicative Functor의 합성을 구현하라.
    def compose[G[_]](G: Applicative[G]): Applicative[({type f[X] = F[G[X]]})#f] = {
      val self = this
      new Applicative[({type f[X] = F[G[X]]})#f] {
          def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
          override def map2[A,B,C](ma: F[G[A]], mb: F[G[B]])(f: (A,B) => C): F[G[C]] = {
            self.map2(ma,mb)((a,b) => G.map2(a,b)(f(_,_)))
          }
      }
    }
      
    def apply[A, B](fo: F[A => B])(m: F[A]): F[B] =
      map2(m, fo)((a, f) => f(a))
    
  }
  
  //################ ApplicativeV2 ############################
  //ex-09) 기본수단을 unit과 map2대신 unit과 apply 로 구성하는 적용함자를 구성하라. 
  trait ApplicativeV2[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

    //ex-10)map 를 unit과 apply로 구현하라.
    def map[A, B](m: F[A])(f: A => B): F[B] =
      apply(unit(f))(m)

    //ex-11)map2를 map과 apply로 구현하라.
    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      apply(map(ma)(f.curried))(mb) // map(ma)(f.curried) 은 F[B => C] 가 return
      
    //ex-13) map3를 uint,apply,curried 를 이용하여 구현하라.
    def map3[A, B, C, D](ma: F[A], mb: F[B], mc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(apply(unit(f.curried))(ma))(mb))(mc)

    //ex-14) map4를 unit,apply,curried 를 이용하여 구현하라.
    def map4[A, B, C, D, E](ma: F[A], mb: F[B], mc: F[C], md: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(apply(unit(f.curried))(ma))(mb))(mc))(md)
      
    //ex-22) Monoid 에서 prudct를 통행 2개의 Monid를 받아 두 Monoid를 합처서 1개의 Monoid를 만드므로써 여러기능을 한번에 
    //처리 할 수 있게 했다.
    //그와 같이 Applicative Fuctor에 다음의 함수를 구현하라.
    def productG[G[_]](G: ApplicativeV2[G]): ApplicativeV2[({type f[X] = (F[X],G[X])})#f] = { 
        val self = this
        new ApplicativeV2[({type f[X] = (F[X],G[X])})#f] {
          def unit[A](a: => A): (F[A],G[A]) = (self.unit(a),G.unit(a))
          
          override def apply[A,B](fab: (F[A => B],G[A => B]))(fa: (F[A],G[A])): (F[B],G[B]) = 
            (self.apply(fab._1)(fa._1),G.apply(fab._2)(fa._2))
            
        }
    }
    
  }

  //################ ApplicativeV3 ############################
  trait ApplicativeV3[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def map[A, B](a: F[A])(f: A => B): F[B]
    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C]

    //ex-12) apply를 map2와 map으로 구현하라.
    def apply[A, B](fo: F[A => B])(m: F[A]): F[B] =
      map2(m, fo)((a, f) => f(a))

      
    //ex-24) Applicative trait에 sequence를 List가 아니라 Map에 대하여 적용하라.
    // 이런 형식유형마다 sequence를 만들 수 없으므로 이를 일반화 한다.
    def sequenceMap[K,V](xs: Map[K,F[V]]): F[Map[K,V]] = 
      xs.foldLeft[F[Map[K,V]]](unit(Map.empty)){ 
        case (acumF,(k,fv)) => map2(acumF,fv)((a,v) => a + (k -> v) )
      }  
      
  }
  
  
  //################ Traverse ############################
  /*
  ex-25) sequence나 traverse를 일반화 한 trait를 만들어라.
  def traverse[A, B](xs: List[A])(f: A => F[B]): F[List[B]] =
  def sequenceByTraverse[A](xs: List[F[A]]): F[List[A]] =
  */
  trait Traverse[F[_]] {
    def traverse[G[_]: Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] = ???
    def sequence[G[_]: Applicative,A](fga: F[G[A]]): G[F[A]] = ???
  }
  
  //ex-29) traverse 함수를 이용해서 map을 Traverse[F] trait의 한 method로 구현하라.
  //그러면 Traverse가 Functor의 확장이고 traverse 함수가 map의 일반화임을 알 수 있다.
  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] =
      join(map(ma)(f))
  
    override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
      flatMap(mf)(f => map(ma)(f))
  
    override def map[A,B](m: F[A])(f: A => B): F[B] =
      flatMap(m)(a => unit(f(a)))
  
    override def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))
  
    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => flatMap(f(a))(g)
  
    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
  }
  
  trait TraverseV2[F[_]] extends Functor[F]{
    def traverse[G[_],A,B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]]
    def sequence[G[_],A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]]
    
    type Id[A] = A

    val idMonad = new Monad[Id] {
      def unit[A](a: => A) = a
      override def flatMap[A,B](a: A)(f: A => B): B = f(a)
    }
    
    def map[A,B](fa: F[A])(f: A => B): F[B] = 
      traverse[Id, A, B](fa)(f)(idMonad)
  }
  
  //ex-30) Foldalbe 의 foldMap도 traverse 로 구현하라. 
  import basic.monoid.Foldable
  import basic.monoid.Monoid
  trait TraverseV3[F[_]] extends Functor[F] with Foldable[F] {
    def traverse[G[_],A,B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]]
    def sequence[G[_],A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]]
    
    type Id[A] = A

    val idMonad = new Monad[Id] {
      def unit[A](a: => A) = a
      override def flatMap[A,B](a: A)(f: A => B): B = f(a)
    }
    
    def map[A,B](fa: F[A])(f: A => B): F[B] = 
      traverse[Id, A, B](fa)(f)(idMonad)
    
    type Const[M,B] = M
    def monoidApplicative[M](M: Monoid[M]) = 
      new Applicative[({type f[X] = Const[M,X]})#f] {
        def unit[A](a: => A): M =  M.zero
        def map2[A,B,C](m1: M, m2: M)(f: (A,B) => C): M = M.op(m1,m2)
      }
    
    override def foldMap[A,B](xs: F[A])(f: A => B)(m: Monoid[B]): B = 
      traverse[({type f[x] = Const[B,x]})#f,A,Nothing](xs)(f)(monoidApplicative(m))
      
  }
  
  //ex-15) Either를 위한 Monad instance를 만들어라.
  def eitherMonad[E]:basic.monad.Monad[({ type f[X] = Either[E, X] })#f] = 
    new basic.monad.Monad[({ type f[X] = Either[E, X] })#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)
      def flatMap[A, B](m: Either[E, A])(f: A => Either[E, B]): Either[E, B] = m match {
        case Right(a) => f(a)
        case Left(e)  => Left(e)
    }
  }
  
  //ex-16) 다음의 validation을 위한 Applicative functor를 구현하라.
  trait Validation[+E,+A]
  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E,Nothing]
  case class Success[A](a: A) extends Validation[Nothing,A]
  
  def validator[E] = new Applicative[({ type f[X] = Validation[E,X]})#f] {
      
      def unit[A](a: => A): Validation[E,A] = Success(a)
      
      // flatMap으로 구현된 것이 아니기 때문에 mb는 ma와 상관없이 상자에서 풀어 무언가를 할 수 있다.
      def map2[A,B,C](ma: Validation[E,A], mb: Validation[E,B])(f: (A,B) => C): Validation[E,C] = (ma,mb) match {
        case (Success(a),Success(b)) => Success(f(a,b))
        case (Failure(h1,t1),Failure(h2,t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2) 
        case (e @ Failure(_,_), _) => e
        case (_, e @ Failure(_,_)) => e
      }
      
  }
  
}

object ApplicativeDriver extends App {

  import basic.applicative.ApplicativeStudy._
  
  //ex-19) Option Applicative 를 이용하여  
  //applicative functor의 map2 함수에 대하여 왼쪽항등, 오른쪽 항등이 증명을 통해 구조적 보전의 Functor law를 증명하라.
  val opa = new Applicative[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def map2[A,B,C](ma: Option[A], mb: Option[B])(f: (A,B) => C): Option[C] = (ma,mb) match {
      case (Some(a), Some(b)) => Some(f(a,b))
      case (_,_) => None
    }
  }
  
  val rs01 = opa.map2(opa.unit(()), Some("a"))((_,a) => a) == Some("a")
  println(rs01)
  val rs02 = opa.map2(Some("a"),opa.unit(()))((a,_) => a) == Some("a")
  println(rs02)
  
  //ex-20) map2의 결합법칙을 증명하라.
  // op(a,op(b,c)) = op(op(a,b),c)
  //map2(a,map2(b,c)) = map2(map2(a,b),c)
  val rs03 = opa.map2(Some(3), opa.map2(Some(4),Some(5))((a,b) => a + b))((a,b) => a + b) == 
    opa.map2(opa.map2(Some(3), Some(4))((a,b) => a + b ), Some(5))((a,b) => a + b)
    
  println(rs03)
  
  //ex-21)곱의 자연성 법칙를 증명하라.
  //map2 함수에 인자를 적용하기전에 map함수를 적요하고 인자로 적용하나 map2안에서 map함수를 적용하나 동일 하다.
  //map2(a,b)(productF(f,g)) == product(map(a)(f), map(b)(g))
  val rs04 = opa.map2(Some(2), Some(3))(opa.productF(a => a * 2, b => b * 3)) == 
    opa.product(opa.map(Some(2))(a => a * 2), opa.map(Some(3))(b => b * 3))
  println(rs04)
  
 
  
 
  //ex-26) List에 대한 Traverse instance를 만들어라.
  val listTraverse= new Traverse[List] {
    override def traverse[G[_],A,B](xs: List[A])(f: A => G[B])(implicit G:Applicative[G]): G[List[B]] = 
      xs.foldRight[G[List[B]]](G.unit(List()))((a,g) => G.map2(g, f(a))((g1,a1) => a1 :: g1))
    
  }
  
  //ex-27) Option에 대한 Traverse instance를 만들어라.
  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_],A,B](xs: Option[A])(f: A => G[B])(implicit G:Applicative[G]): G[Option[B]] = xs match {
      case Some(a) => G.map(f(a))(Some(_))
      case None => G.unit(None)
    }
  }
  
  //ex-28) 다음 Tree에 대한 구조에 대하여 Traverse instance를 작성하라.
  case class Tree[+A](h: A, t: List[Tree[A]])
  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_],A,B](xs: Tree[A])(f: A => G[B])(implicit G:Applicative[G]): G[Tree[B]] = 
      G.map2(f(xs.h), listTraverse.traverse(xs.t)(a => traverse(a)(f)))(Tree(_,_))
      
  }
  
  val validateName = Right("sslee")
  val validateAge = Right(42)
  val validateEmail = Right("sslee05@gmail.com")

  val m:basic.monad.Monad[({ type f[X] = Either[Any, X] })#f] = eitherMonad

  val rs = m.flatMap(validateName)(a => m.flatMap(validateAge)(b => m unit b))
  println(rs)
  
  val validateFailName = Left("not found name")
  val rs2 = m.flatMap(validateFailName)(a => m.flatMap(validateAge)(b => m unit b))
  println(rs2)
  
  def tra[G[_],A,B](xs: List[A])(f: A => G[B]): G[List[A]] = ???
}

