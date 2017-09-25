package basic.monad

import basic.parallel.Par
import basic.parallel.Par._
import basic.state.State
import basic.state.State._


object MonadStudy {
  
  //############### Functor ###############
  //Par 와 Option의 map 함수들 
  def map[A,B](par: Par[A])(f: A => B): Par[B] = ???
  def map[A,B](o: Option[A])(f: A => B): Option[B] = ???
  
  /**
   위의 map함수를 보면  f: A => B 를 받아 Par[A] => Par[B] 의 기능를 한 것이 되었다.
   즉 f: A => B를 받고 Par[A] 를 받아 Par[B] 이 되었다.
   사상을 받아 사상의 기능을 했다. 이를 일반화 하여 나타내면 다음과 같다.
   그리고 이를 Functor라 하며, 이는 일반 사상(mapping)과는 다르다. Functor는 function의 
   구조를 깨지 안는다. 
   function의 조건 = 구조를 보존한다.
    - 모든 정의역의 원소에 해당하는 치역의 원소가 있다.
    - 정의역의 원소 x는 항상 1개의 치역의 원소이다.
   */
  //ex-01) Functor를 modeling 하라.
  trait Functor[F[_]]  {
    def map[A,B](fo: F[A])(f: A => B): F[B]
    
    //map으로 다른 일을 할 수 있다.
    def distribute[A,B](fo: F[(A,B)]):(F[A],F[B]) = 
      (map(fo)(a => a._1) , map(fo)(b => b._2) )
      
    def codistribute[A,B](e: Either[F[A],F[B]]): F[Either[A,B]] = e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
  }
  
  //ex-02) ListFunctor를 생성하라.
  val listFunctor = new Functor[List] {
    def map[A,B](xs: List[A])(f: A => B): List[B] = 
      xs map f
  }
  
  //############### Monad ###############
  // 지금까지 한 것 
  def map2[A,B,C](o1: Option[A],o2: Option[B])(f: (A,B) => C): Option[C] =
    o1 flatMap(a1 => o2 map(b1 => f(a1,b1)))
  
  def map2[A,B,C](p1: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] = 
    p1 flatMap(a1 => Par.map(p2)(b1 => f(a1,b1)))
  
  // 공통점 뽑기 
  // map,flatMap을 기본수단으로 하고 이들을 이용하여 map2를 구현 할 수 있다.
  trait MonadV01[F[_]] {
    def map[A,B](fo: F[A])(f: A => B): F[B]
    def flatMap[A,B](fo: F[A])(f: A => F[B]): F[B]
    
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = 
      flatMap(fa)(a1 => map(fb)(b1 => f(a1,b1)))
  }
  
  // 기본수단으로 되어 있는 것 들 중에서 기본수단으로 더 만들 수 있는 함수인지 생각 해보자.  
  //map은 flatMap 과 unit으로 구현 할 수 있다 => map를 기본수단에서 제거 해보자.
  trait MonadV02[F[_]] {
    def unit[A](a: => A): F[A]
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
    
    def map[A,B](fa: F[A])(f: A => B): F[B] = 
      flatMap(fa)(a => unit(f(a)))
      
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = 
      flatMap(fa)(a1 => map(fb)(b1 => f(a1,b1)))
  }
  
  /**
   위와 같이 최소한의 기본수단 집합은 unit 과 flatMap이어야 한다.
   이제부터 할 일은 이 함수들이 정의되어 있는 모든 자료 형식을 하나의 개념으로 묶는 것이고,
   이를 Monad라 하자. 
   이 Monad trait는 기본수단으로 unit 과 flatMap를 가지고 , map,map2을 기본 구현으로 둔다.
   따라서 Monad는 extends Functor 관계가 된다.
   */
  //ex-03) Monad trait를 modeling화 하되 unit,flatMap을 기본수단으로 하고, map과 map2를 기본함수로 구현하라. Functor와의 관계를 성립하라.
  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A,B](a: F[A])(f: A => F[B]): F[B]
    
    def map[A,B](a: F[A])(f: A => B): F[B] =
      flatMap(a)(a1 => unit(f(a1)))
    def map2[A,B,C](a: F[A], b: F[B])(f: (A,B) => C): F[C] = 
      flatMap(a)(a1 => map(b)(b1 => f(a1,b1)))
  }
 
  
  //Par Monad 만들어 보기 
  // unit 과 flatMap를 만들면 map,map2 는 저절로 얻을 수 있다.
  //ex-04) Par에 대한 monad를 작성하라.
  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a) 
    def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] = 
      Par.flatMap(p)(a => f(a))
  }
  
  //ex-05) List에 대한 monad를 작성하라.
  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List[A]()
    def flatMap[A,B](xs: List[A])(f: A => List[B]): List[B] = 
      xs flatMap f
  }
  
  //ex-06) Option에 대한 monad를 작성하라.
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A,B](o: Option[A])(f: A => Option[B]): Option[B] = 
      o flatMap f
  }
  
  //ex-07) Stream에 대한 monad를 작성하라.
  val tt = Stream(1,2,3)
  val stream = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A,B](s: Stream[A])(f: A => Stream[B]): Stream[B] = 
      s flatMap f
  }
  
  //ex-08) State에 대한 monad를 작성하라.
  class StateMonad[S] {
    type StateS[A] = State[S,A]
    
    val monad = new Monad[StateS] {
      def unit[A](a: => A):StateS[A] = State.unit(a)
      def flatMap[A,B](s: StateS[A])(f: A => StateS[B]): StateS[B] = 
        s flatMap f
    }
  }
  
  //inline으로 하면 위에 StateMonad class가 필요 없다.
  def stateMonad[S] = new Monad[({type StateS[A] = State[S,A]})#StateS] {
    def unit[A](a: => A): State[S,A] = State.unit(a)
    def flatMap[A,B](s: State[S,A])(f: A => State[S,B]): State[S,B] = 
      s flatMap f
  }
  
  //monad 추가 확장 
  trait MonadV03[F[_]] {
    def unit[A](a: => A): F[A]
    def flatMap[A,B](fo: F[A])(f: A => F[B]): F[B]
    
    def map[A,B](fo: F[A])(f: A => B): F[B] = 
      flatMap(fo)(a => unit(f(a)))
      
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = 
      flatMap(fa)(a => map(fb)(b => f(a,b)))
      
    //ex-09) sequence를 monad로 일반화 하라. 
    def sequence[A](xs: List[F[A]]): F[List[A]] = 
      xs.foldRight[F[List[A]]](unit(List()))((a,b) => map2(a,b)((a1,b1) => a1::b1))
      
    //ex-10) traverse를 monad로 일반화 하라.
    def traverse[A,B](xs: List[A])(f: A => F[B]): F[List[B]] = 
      xs.foldRight[F[List[B]]](unit(List()))((a,b) => map2(f(a),b)((a1,b1) => a1::b1))
      
    //ex-11) replicate를 monad로 일반화 하라.
    def replicate[A](n: Int, ma: F[A]): F[List[A]] = 
      if(n <= 0) unit(List[A]()) else map2(ma,replicateM(n-1,ma))((a,b) => a::b) 
    
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = 
      sequence(List.fill(n)(ma))
    
    //ex-12) 2개의 자료구조를 곱하는 함수를 작성하라.
    def product[A,B](ma: F[A], mb: F[B]): F[(A,B)] = 
      map2(ma,mb)((a,b) => (a,b))
      
    //ex-13) filterM를 구현하라. 이는 A => Boolean 이것이 아니라 A => F[Boolean] 가 주어진다.
    def filterM[A](xs: List[A])(f: A => F[Boolean]): F[List[A]] = xs match {
        case Nil => unit(List())
        case h::t => flatMap(f(h))(b => if(b) map(filterM(t)(f))(t1 => h::t1)  else filterM(t)(f) )
    }
    
    //########## Monad 의 결합 법칙 ##################
    /**
      x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
      Some(v) 에 대하여 적용 해보자
      1)Some(v).flatMap(f).flatMap(g) == Some(v).flatMap(a => f(a).flatMap(g))
      
      	f(v).flatMap(g) 		      <== Option(v).flatMap(f(v)) <== f:v => Option(v)
      	a => f(a).flatMap(g)      <== a 를 받아 return type이 f(a).flatMap(g)
        (a => f(a).flatMap(g))(v) <== Some(v).flatMap(...) == ...(v)  
        
      2) f(v).flatMap(g) == (a => f(a).flatMap(g))(v)
        v => f(v)                 <== a == v
      3) f(v).flatMap(g) == f(v).flatMap(g)
     */
    
    
    //ex-14) Kleisli arrow 에 관한 compose 함수를 구현하라.
    def compose[A,B,C](f: A => F[B],g: B => F[C]): A => F[C] = 
      a => flatMap(f(a))(g)
   
    def mapT[A,B](fo: F[A])(f: A => B): F[B] = 
      flatMap(fo)(a => unit(f(a)))  
      
    /**
     	Monoid의 결합법칙의 정의는 simple하고 알아보기 쉽다.
      op(op(a,b),c) == op(a,op(b,c))
      
      이처럼 Monad도 그렇게 표현 하려면  위의 F[A](Option[A]) 같은 형식의 값이 아닌 A => F[A] 처럼 형식의 함수를 고려하면
      쉽게 표현 가능하다.
      compose(compose(f,g),h) == compose(f,compose(g,h))  
     */
    
    //ex-15) flatMap를 compose를 이용하여 구현하라.
    def flatMapViaCompose[A,B](a: F[A])(f: A => F[B]): F[B] = 
      compose( (_:Unit) => a,f)(())
      
    // 최소 기본구현이 unit,flatMap 에서 unit 과 compose로 변경이 되었다.
      
    //ex-16) compose(compose(f,g),h) == compose(f,compose(g,h)) 의 결합법칙을 flatMap을 이용하여 증명하 
    
    /**
     1) compose(compose(f,g),h) == compose(f,compose(g,h))
     	  a => flatMap(compose(f,g)(a))(h)   		<== compose(f,g) == a => flatMap(f(a))(g)(a)
     
     2) a => flatMap(compose(f,g)(a))(h) == compose(f,b => flatMap(g(b))(h))
     
     3) a => flatMap( (b => flatMap(f(b))(g))(a) )(h) == a => flatMap(f(a))(b => flatMap(g(b)(h))
     		b 			<==  b == a
     
     4) a => flatMap(flatMap(f(a))(g))(h) == a => flatMap(f(a))(b => flatMap(g(b))(h))
     		f(a) 를 x 라 하자.
     		
     5) a => flatMap(flatMap(x)(g))(h) == a => flatMap(x)(b => flatMap(g(b))(h))
        
     
     6) flatMap(flatMap(x)(g))(h) = flatMap(x)(b => flatMap(g(b))(h))
        g 를 f 로 이름을 변경 
        h 를 g 로 이름을 변경 
        b 를 a 로 이름을 변경
     
     6) flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))
        x.flatMap(f).flatMap(g)   == x.flatMap(a => f(a).flatMap(g))
		*/
      
    //################ Monad의 항등원의 법칙 ##################
    /**
     compose의 항등원은 unit 이다.
     def unit[A](a: => A): F[A]
     
     left unit 항등법칙
     compopse(unit,f) == f
     rignt unit 항등법칙
     compose(f,unit) == f
     
     
     flatMap(unit(y))(f) = f(y) 
     flatMap(x)(unit) == x 
     */
      
     //ex-17) compose(f,unit)(v) == f(v) 임을 증명하라.
     /**
     (a => flatMap(f(a))(unit)) (v)
     flatMap(f(v))(unit)
     flatMap(X)(unit) = X
      */
      
     //ex-18) compose(unit,f)(v) == f(v) 임을 증명하라.
     /**
     (a => flatMap(unit(a))(f))(v)
     flatMap(unit(v))(f) = f(v)
      */
     
     //ex-19) Monad하나를 선택해서, 그 Monad에 대해 항등법칙이 성립함을 증명하라.
     /**
     flatMap(None)(Some(_)) == None
     None == None
     
     flatMap(Some(v))(Some(_)) == Some(v)
     Some(v) = Some(v)
     
     flatMap(Some(None))(f) == f(None)
     f(None) = f(None)
     
     flatMap(Some(Some(v)))(f) == f(Some(v))
     f(Some(v)) = f(Some(v))
      */
     
     //ex-16) join 를 flatMap를 이용하여 구현하라.
     def join[A](mma: F[F[A]]): F[A] = 
       flatMap(mma)(ma => ma)
     
     //ex-17) flatMap을 join 과 map를 이용하여 구현하라.
     def flatMapViaJoin[A,B](a: F[A])(f: A => F[B]): F[B] = 
       join(map(a)(a1 => f(a1)))
       
     //ex-18) compose를 join 과 map를 이용하여 구현하라.
     def composeViaJoin[A,B,C](f: A => F[B])(g: B => F[C]):A =>  F[C] =
       a => join(map(f(a))(g))
       
     
       
     //################ Monad 정리  ##################
     /**
     지금까지 본 것으로 Monad 의 조합기의 최소 set(집합) 은 3가지 유형이다.
     1) unit, flatMap
     2) unit,compose
     3) unit,map,join
     
     그리고 Monad의 법칙 3개 가있다.
     m bind f bind g = m bind (a => f(a) bind g)
     bind(unit,f) = f
     bind(f, unit) f
     
     Monad는 Monad적 조합기들의 최소 집합 중 하나의 set를 결합법칙,항등법칙을 만족하도록 구현한 것이다.
     
     Monad의 목적 
     한두 형식을 일반화하는 것이 아니라, Monad interface와 법칙을 만족할 수 있는 아주 다양하고 많은 자료 형식을 일반화 한다.
     Monad를 이용하면 언뜻 보기에는 공통점이 전혀 없는 서로 다른 자료형식들에 대한 여러 조합기를 단 한번만 작성할 수 있다.
      */
       

  }
  
  case class Reader[R,A](run: R => A)
  object Reader {
    def readerMonad[R] = new Monad[({type f[X] = Reader[R,X]})#f] {
      def unit[A](a: => A): Reader[R,A] = Reader(r => a)
      def flatMap[A,B](rd: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = 
        Reader(r => {
          val a = rd run r
          f(a).run(r)
        })
    }
  }
  
}



//################ monad 유형중 하나인 항등 monad ##################
case class Id[A](value: A) {
  def unit(a: A): Id[A] = Id(a)
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}


case class User(id: Int, name: String)
case class Comment(user: User, content: String)



object MonadDriver extends App {
  //import basic.monad.MonadStudy.MonadV03.
  import basic.monad.MonadStudy._

  val s1 = Some(1)
  val s2 = Some(2)
  
  val sf1 = (v: Int) => Some(v)
  val sf2 = (v:Int) => Some(v * v)
  val sf3 = (v:Int) => Some(v.toDouble / 2.toDouble) 
  val m3 = new MonadV03[Option] {
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