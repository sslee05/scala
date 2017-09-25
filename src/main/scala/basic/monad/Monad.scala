package basic.monad

//ex-01) Functor를 modeling 하라.
/*
f: A => B를 받고 F[A] 를 받아 F[B] 이 되었다.
사상을 받아 사상의 기능을 했다. 이를 일반화 하여 나타내면 다음과 같다.
그리고 이를 Functor라 하며, 이는 일반 사상(mapping)과는 다르다. Functor는 function의 
구조를 깨지 안는다. 
function의 조건 = 구조를 보존한다.
- 모든 정의역의 원소에 해당하는 치역의 원소가 있다.
- 정의역의 원소 x는 항상 1개의 치역의 원소이다.
*/

trait Functor[F[_]] {
  def map[A,B](ma: F[A])(f: A => B): F[B]
  
  //ex-02)map 으로 할 수 있는 다음을 구현하라.
  def distribute[A,B](ma: F[(A,B)]): (F[A],F[B]) =
    (map(ma)(t => t._1 ), map(ma)(t => t._2))
    
  //ex-03)map 으로 할 수 있는 다음을 구현하라.
  def codistribute[A,B](e: Either[F[A],F[B]]): F[Either[A,B]] = e match {
    case Left(l) => map(l)(d => Left(d))
    case Right(r) => map(r)(d => Right(d))
  }

}

object Functor {
  
  //ex-04) 다음을 구현하라.
  def listFunctor: Functor[List] = new Functor[List] {
    def map[A,B](ma: List[A])(f: A => B): List[B] = 
      ma map(a => f(a))
  }
}

//ex-04) unit과 flatMap를 기본수단으로 하는 Monad를 구현하라.
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
  
  //ex-05) flatMap과 unit으로 map를 구현하라.
  def map[A,B](ma: F[A])(f: A => B): F[B] = 
    flatMap(ma)(a => unit(f(a)))
    
  //ex-06) flatMap과 map으로 map2를 구현하라.
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] = 
    flatMap(ma)(a => map(mb)(b => f(a,b)))
  
  //ex-12) sequence를 구현하라.
  def sequence[A](xs: List[F[A]]): F[List[A]] = 
    xs.foldRight[F[List[A]]](unit(List()))((a,b) => map2(a,b)((a1,b1) => a1 :: b1))
    
  //ex-13) traverse 를 구현하라.
  def traverse[A,B](xs: List[A])(f: A => F[B]): F[List[B]] = 
    xs.foldRight[F[List[B]]](unit(List()))((a,b) => map2(f(a),b)((a1,b1) => a1 :: b1))
    
  //ex-14) replicate 를 구현하라.
  def replicate[A](n: Int, ma: F[A]): F[List[A]] = 
    if(n > 0) map2(ma,replicatedM(n-1,ma))((a,b) => a :: b)
    else unit(List[A]())
  
  def replicatedM[A](n: Int, ma: F[A]): F[List[A]] = 
    sequence(List.fill(n)(ma))
    
  //ex-15)map2를 이용하여 2개의 자료구조를 곱하는 함수를 작성하라.
  def product[A,B](ma: F[A], mb: F[B]): F[(A,B)] =
    map2(ma,mb)((a,b) => (a,b))
    
  //ex-16)filterM를 구현하라. 이는 A => Boolean 이것이 아니라 A => F[Boolean] 가 주어진다.
  def filterM[A](xs: List[A])(p: A => F[Boolean]): F[List[A]] = xs match {
      case Nil => unit(Nil)
      case h::t => flatMap(p(h))(a => if(a) map(filterM(t)(p))(t1 => h :: t1) else filterM(t)(p) )
  }
  
  //ex-17)Kleisli arrow 에 관한 compose 함수를 구현하라.(Monad Set02: unit,compose)
  def compose[A,B,C](f: A => F[B],g: B => F[C]): A => F[C] = 
    a => flatMap(f(a))(g)
    
  //ex-19) flatMap을 compose로 구현하라.(Monad Set02: unit,compose 증명)
  def flatMapViaCompose[A,B](ma: F[A])(f: A => F[B]): F[B] = 
    compose((_:Unit) => ma, f)(())
   
  //ex-20) join 함수를 구현하라.(Monad Set03: unit,map,join)
  def join[A](mma: F[F[A]]): F[A] = 
    flatMap(mma)(ma => ma)
    
  //ex-21) flatMap을 join과 map으로 구현하라.(Monad Set03: unit,map,join 증명)
  def flatMapViaJoin[A,B](ma: F[A])(f: A => F[B]): F[B] = 
    join(map(ma)(a => f(a)))
    
  //ex-22)compose를 join 과 map를 이용하여 구현하라
  def composeViaJoin[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = 
    a => join(map(f(a))(g)) 
}

object Monad {
  
  //ex-07)Par에 대한 Monad instance를 생성하라.
  import basic.parallel.Par._
  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = unit(a)
    def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] = 
      ma flatMap f
  }
  
  //ex-08) List에 대한 Monad instance를 생성하라.
  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A,B](xs: List[A])(f: A => List[B]): List[B] =
      xs flatMap f
  }
  
  //ex-09) Option에 대한 Monad instance를 생성하라.
  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A,B](o: Option[A])(f: A => Option[B]) = 
      o flatMap f
  }
  
  //ex-10) Stream에 대한 Monad instance를 생성하라.
  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A,B](s: Stream[A])(f: A => Stream[B]): Stream[B] = 
      s flatMap f
  }
  
  //ex-11) State에 대한 Monad instance를 생성하라.
  import basic.state.State._
  import basic.state.State
  def stateMonad[S]: Monad[({type f[x] = State[S,x]})#f] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S,A] = State.unit(a)
    def flatMap[A,B](s: State[S,A])(f: A => State[S,B]): State[S,B] = 
      s flatMap f
  }
  
}

case class Reader[R,A](run: R => A)
object Reader {
  
 //ex-23) Reader Monad를 생성하라.
  def readerMonad[R]: Monad[({type f[x] = Reader[R,x]})#f]  = new Monad[({type f[x] = Reader[R,x]})#f] {
   def unit[A](a: => A): Reader[R,A] = Reader(r => a)
   def flatMap[A,B](rd: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = 
      Reader(r => {
        val a = rd run r
        f(a).run(r)
      })
  }
}

//ex-24) 항등 Monad case class를 작성하라.
case class Id[A](value: A) {
  def unit(a: A): Id[A] = Id(a)
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}


object MonadDriver extends App {
  //import basic.monad.MonadStudy.MonadV03.
  import basic.monad.Monad._

  val s1 = Some(1)
  val s2 = Some(2)
  
  val sf1 = (v: Int) => Some(v)
  val sf2 = (v:Int) => Some(v * v)
  val sf3 = (v:Int) => Some(v.toDouble / 2.toDouble) 
  val m3 = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A,B](fo: Option[A])(f: A => Option[B]): Option[B] = 
      fo.flatMap(a => f(a))
  }
  val rx02 = m3.compose(m3.compose(sf2,sf1), sf3)(3)
  println(rx02)
  
  val rx03 = m3.compose(sf2,m3.compose(sf1, sf3))(3)
  println(rx03)
  
  
  def mapF[A,B,C](f: A => B)(g: B => C): A => C = g compose f
  
  val fb = Some((a:Int) => a * a)
  val vb = Some(2)
  
  def mapTest[A,B](ma: Option[A], f: Option[A => B]): Option[B] = 
    ma.flatMap(a => f.map(b => b(a)))
    
  println(mapTest(vb,fb))
  
  val f = (v:Int) => Some(v * v)
  val g = (v:Int) => Some(v.toDouble / 2.toDouble) 
  
  s1.flatMap(a => f(a).flatMap(g))
  def test(m: Option[Int]): Option[Double] =  m match {
    //case Some(v) => ((a:Int) => f(a).flatMap(g))(v)
    case Some(v) => f(v).flatMap(g)
  }
  
}
