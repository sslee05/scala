package exercise.applicative

import basic.monad.Functor

//ex-01) Applicative trait 를 만들어라 
trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C]
  
  //ex-02) unit 과 map2를 이용하여 map를 구현하라.
  def map[A,B](ma: F[A])(f: A => B): F[B] = ???
    
  //ex-03)traverse 함수를 구현하라.
  def traverse[A,B](xs: List[A])(f: A => F[B]): F[List[B]] = ???
    
  //ex-04)traverse를 이용하여  sequence 함수를 구현하라.
  def sequenceViaTraverse[A](xs: List[A]): F[List[A]] = ???
    
  //ex-04)map2 와 unit 으로 sequence 함수를 구현하라.
  def sequence[A](xs: List[F[A]]): F[List[A]] = ???
    
  //ex-05) map2 와 unit 으로 replicate 함수를 구현하라.
  def replicate[A](n: Int, ma: F[A]): F[List[A]] = ???
  
  //ex-06) sequence를 이용하여 replicate를 구현하라.
  def replicateViaSequnce[A](n: Int, ma: F[A]): F[List[A]] = ???
    
  //ex-07) map2로 product 함수를 구현하라.
  def product[A,B](ma: F[A], mb: F[B]): F[(A,B)] = ???
    
  //ex-08) apply 를 선언하라. Applicative 기본 Set02 (unit,apply) 
  //ex-13) apply를 map2로 구현하라.
  def apply[A,B](mab: F[A => B])(ma: F[A]): F[B] = ???
  
  //ex-09) map를 unit과 apply로 구현하라. Applicative 기본 Set02(unit,applu)
  def mapViaApply[A,B](ma: F[A])(f: A => B): F[B] = ???
  
  //ex-10) map2를 unit과 apply로 구현하라. Applicative 기본 Set02 (unit,apply)
  def map2ViaApply[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] = ???
    
  //ex-11) map3를 unit과 apply로 구현할.
  def map3[A,B,C,D](ma: F[A], mb: F[B], mc: F[C])(f: (A,B,C) => D): F[D] = ???
    
  //ex-12) map4를 unit과 apply로 구현하라.
  def map4[A,B,C,D,E](ma: F[A], mb: F[B], mc: F[C], md: F[D])(f: (A,B,C,D) => E): F[E] = ???
    
  //ex-15) 쌍들의 묶음을 바꾸는 다음의 함수를 구현하라.
  def assoc[A,B,C](p: (A,(B,C))): ((A,B),C) = ???
  
  //ex-16)각각의 Input => Output 2개를 받아 Input 쌍을 받고 Outout 쌍을 받는 함수를 구현하라.
  def productF[I,O,I2,O2](f: I => O,g: I2 => O2): (I,I2) => (O, O2) = ???
    
  //ex-17) Applicative 합성을 구현하라. 
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[X] = F[G[X]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] = ???
      def map2[A,B,C](ma: F[G[A]], mb: F[G[B]])(f: (A,B) => C): F[G[C]] = ???
    }
  }
    
}

object Applicative {
  //ex-14)다음의 validation을 위한 Applicative functor를 구현하라.
  def validator[E]: Applicative[({type f[x] = Validation[E,x]})#f] = new Applicative[({type f[x] = Validation[E,x]})#f] {
    def unit[A](a: => A): Validation[E,A] = ???
    def map2[A,B,C](ma: Validation[E,A], mb: Validation[E,B])(f: (A,B) => C): Validation[E,C] = ???
  }
  
  def eitherMonad[E]:basic.monad.Monad[({ type f[X] = Either[E, X] })#f] = 
    new basic.monad.Monad[({ type f[X] = Either[E, X] })#f] {
      def unit[A](a: => A): Either[E, A] = ???
      def flatMap[A, B](m: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ???
  }
}

trait Validation[+E,+A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E,Nothing]
case class Success[A](a: A) extends Validation[Nothing,A]

object ApplicativeDriver extends App {

  
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
  
  
  import exercise.applicative.Applicative._
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

