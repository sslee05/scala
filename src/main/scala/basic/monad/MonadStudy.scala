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
    def distribute[A,B](fo: F[(A,B)])(f: A => B):(F[A],F[B]) = ???
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
     	  a => flatMap(compose(f,g)(a),h)   		<== compose(f,g) == a => flatMap(f(a))(g)(a)
     
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
     
     왼쪽 항등법칙
     compose(f,unit) == f
     오른쪽 항등법칙
     compopse(unit,f) == f
     
     flatMap(x)(unit) == x 
     flatMap(unit(y))(f) = f(y) 
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
  }
  
}

object MonadDriver extends App {
  //import basic.monad.MonadStudy.MonadV03._
  
  
}