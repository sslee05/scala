package basic.laziness

import basic.laziness.Stream._

sealed trait Stream[+A] {
  
  def toList:List[A] = {
    type B = List[A] => List[A]
    
    @annotation.tailrec
    def go(xs:Stream[A],z:B)(f:(A,B) => B):B= {
      xs match {
        case Empty => z
        case Cons(h,t) => go(t(),f(h(),z))(f)
      }
    }
    go(this,(y:List[A]) => y)((a,g) => x => g(a::x))(Nil:List[A])
  }
  
  def takeOrigin(n:Int):Stream[A] = {
    this match {
      case Empty => empty[A]
      case Cons(h,t) if n > 0 => cons(h(),t().takeOrigin(n-1))
      case Cons(h,_) => cons(h(),empty)
    }
  }
  
  def dropOrigin(n:Int):Stream[A] = {
    this match {
      case Empty => empty[A]
      case Cons(h,t) if n > 0 => t().dropOrigin(n-1)
      case Cons(h,t) => this
    }
  }
  
  def takeWhileOrigin(p:A => Boolean):Stream[A] = {
    this match {
      case Empty => empty[A]
      case Cons(h,t) if(p(h())) => cons(h(),t() takeWhileOrigin p)
      case Cons(_,t) => t().takeWhileOrigin(p)
    }
  }
  
  /**
   * B type 이 들어가는 곳에 call-by-name으로 함으로써
   * loop 실행이 되지 않고 바로 return 된다.
   * 만약 call-by-name을 하지 않았다면 f(A,B) 에서 
   * 함수 인자를 평가하고 나서 인자로 넘어가기 때문에 t().foldRight(z)(f) 
   * 부분이 평가되어야 하고 그로 인해 모든 원소가 소진될때까지 loop를 실행한다.
   */
  def foldRight[B](z: => B)(f:(A, => B) => B):B = {
    this match {
      case Empty => z
      case Cons(h,t) => f(h(),t().foldRight(z)(f))
    }
  }
  
  /**
   * foldRight가 lazy 함으로써 술어를 만족하는 header만 
   * 찾게 된다면 모든 원소까지 탐색할 필요가 없어 진다.
   */
  def exist(p:A => Boolean):Boolean = 
    foldRight(false)((a,g) => p(a) || g)
    
  def forAll(p:A => Boolean):Boolean = 
    foldRight(true)((a,g) => p(a) && g)
    
  def takeWhile(p:A => Boolean):Stream[A] = 
    foldRight(empty[A])((a,g) => if(p(a)) cons(a,g) else g)
    
  def headOption:Option[A] = 
    foldRight(None:Option[A])((a,g) => Some(a))
    
  def map[B](f:A => B):Stream[B] = 
    foldRight(empty:Stream[B])((a,g) => cons(f(a),g))
    
  def append[B >: A](xs: => Stream[B]):Stream[B] = 
    foldRight(xs)((a,g) => cons(a,g)) 
    
  def flatMap[B](f:A => Stream[B]):Stream[B] = 
    foldRight(empty[B])((a,g) => f(a) append g )
  
  def filter(p:A => Boolean):Stream[A] = 
    foldRight(empty[A])((a,g) => if(p(a)) cons(a,g) else g)
  
  //############### infinite #################  
  def mapViaUnfold[B](f: A => B):Stream[B] = 
    unfold(this){
      case Cons(h,t) => Some((f(h()),t()))
      case Empty => None
  }
  
  /**
   * unfold를 이용하여 take 를 구성하라.
   */
  def take(n:Int):Stream[A] = 
    unfold((n,this)){ 
      case (x,Cons(h,t)) if x > 0 => Some((h(),(x-1,t())))
      case (x,Cons(h,t)) if x == 0 => Some(h(),(x-1,empty))
      case _ => None
    }
  
  /**
   * unfold를 이용하여 zipWith를 구성하라 
   */
  def zipWith[B,C](xs:Stream[B])(f:(A,B) => C):Stream[C] = 
    unfold((this,xs)){
      case (Cons(h1,t1),Cons(h2,t2)) => 
        Some((f(h1(),h2()) , (t1(),t2()) ))
      case _ => None
    }
  
  /**
   * unfold를 이용하여 zipAll를 구성하라.
   */
  def zipAll[B](xs:Stream[B]):Stream[(Option[A],Option[B])] = 
    unfold((this,xs)){
      case  (Cons(h1,t1),Cons(h2,t2)) => 
              Some(( (Some(h1()),Some(h2()) ),(t1(),t2()) ))
      case  (Cons(h1,t1),Empty) => 
              Some(( (Some(h1()),None), (t1(),Empty) ))
      case  (Empty,Cons(h2,t2)) => 
              Some(( ( None , Some(h2())) , (Empty,t2()) ))
      case  (Empty,Empty) => None
    }
  
  /**
   * 위의 function 들로 아래와 같은 것을 구현하라.
   * Stream(1,2,3,4,5) 와 Stream(1,2,3) 은 true Stream(2,3,4) 는 false
   */
  def startsWith[B >: A](sub:Stream[B]):Boolean = 
    sub zipAll this takeWhile(x => !x._1.isEmpty) forAll( x => x._1 == x._2 )
    
  /**
   * Stream(1,2,3,4,5) 와 Stream(2,3,4) true Stream(4,5) true
   */
  def subSequence[B >: A](sub:Stream[B]):Boolean = 
    sub.foldRight(true)((x,y) => this.exist(a => a == x) && y )
    
  /**
   * tail를 일반화한 scanRight 함수를 작성하라 
   * Stream(1,2,3) 의 f:(a,b) => a + b의 
   * scanRight의 결과는 Stream(6,5,3,0) 이어야 한다.
   */
  def scanRight[B](z:B)(f:(A, => B) => B):Stream[B] = 
    foldRight((z,Stream(z)))((a,b) => {
      lazy val cal = b // foldRight의 t().foldRight(z)(f):(B,Stream(B))
      val calVal = f(a,cal._1) //call-by-name
      (calVal,cons(calVal,cal._2))//call-by-name
    })._2
}

/**
 * class parameter 인 경우 기술적인 문제로 인하여 이름으로 전달되는 인수가 아니라 
 * 반드시 명시적으로 강제해야 하는 thunk 이어야 한다
 */
sealed case class Cons[+A](head:() => A,tail:() => Stream[A]) extends Stream[A]
object Empty extends Stream[Nothing]

object Stream {
  def cons[A](h: => A, t: => Stream[A]):Stream[A]= {
    lazy val head = h
    lazy val tail = t
    
    Cons(() => head,() => tail)
  }
  
  def empty[A]:Stream[A] = Empty
  
  def apply[A](xs:A*):Stream[A] = {
    if(xs.isEmpty) empty
    else cons(xs.head,apply(xs.tail:_*))
  }
  
  //############### infinite #################
  
  /**
   * 1 의 무한 Stream 을 작성하라.
   */
  def ones:Stream[Int] = cons(1,ones)
  
  /**
   * 주어진 값의 무한 stream을 작성하라.
   */
  def constant[A](v:A):Stream[A] = cons(v,constant(v))
  
  /**
   * n에서 시작해서 n+1,n+2 이어지는 무한정수 stream을
   * 생성하는 함수를 작성하라.
   */
  def from(n:Int):Stream[Int] = 
    cons(n,from(n+1))
    
  /**
   * fibo 수 0,1,1,2,3,5,8,13 ... 로 이루어진
   * 무한 stream를 생성하는 fibs를 작성하
   */
  def fibs:Stream[Int] = {
    
    def go(pre:Int,cur:Int):Stream[Int] = 
      cons(pre,go(cur,pre + cur))
    
    go(0,1)
  }
  
  /**
   * 일반화된 Stream 구축 함수를 작성 하라.
   * 이 함수는 초기상태 하나와 다음 상태 및 다음 값(생성된 Stream 안의)를 
   * 산출하는 함수 하나를 받아야 한다.
   */
  def unfold[A,S](z:S)(f:S => Option[(A,S)]):Stream[A] ={
    f(z) match {
      case Some((h,x)) => cons(h,unfold(x)(f))
      case None => empty[A]
    }
  }
  
  /**
   * unfold를 이용하여 fibs 를 재정의 하라.
   */
  def fibsViaUnfold:Stream[Int] = 
    unfold((0,1)){ case (pre,cur) => Some(pre,(cur,pre + cur))}
  
  
  /**
   * unfold를 이용하여 from를 재정의 하라.
   */
  def fromViaUnfold(n:Int):Stream[Int] = 
    unfold(n)(x => Some((x,x+1)))
    
  /**
   * unfold를 이용하여 constant를 재정의 하라.
   */
  def constantViaUnfold(n:Int):Stream[Int] = 
    unfold(n)(x => Some((n,n)))
  
  /**
   * unfold 를 이용하여 다음을 구성하라. 
   * Stream(1,2,3) 에 대하여 
   * Stream(Stream(1,2,3),Stream(2,3),Stream(3),Stream()) 을 
   * 반환 해야 한다.
   */
  def tails[A](xs:Stream[A]):Stream[Stream[A]] = {
    unfold(xs) {
      case Cons(h,t) => Some((cons(h(),t()) ,t()))
      case Empty => None
    } append empty
  }
  
}

object StreamDriver extends App {
  
  //takeWhileOrigin test
  val xs = Stream(1,4,5,6,3,2)
  println(xs.takeWhileOrigin(x => x > 3).toList)
  
  //exist test
  println(xs.exist(x => x > 3))
  
  //forAll test
  println(xs.exist(x => x < 1))
  
  //takeWhile test
  println(xs.takeWhile(x => x > 3).toList)
  
  //head Option test
  println(xs.headOption)
  
  //test map
  println(xs.map(x => x * 2).toList)
  
  //append test
  val ys = Stream(8,9,10)
  println(xs append ys toList)
  
  //test flatMap
  val xs01 = Stream(Stream(1,2),Stream(3,4),Stream(5,6))
  val rs01 = xs01 flatMap(x => x map(x1 => x1 + 2))
  println(rs01.toList)
  
  //test filter
  println(xs.filter(x => x > 2).toList) 
  
  //test constant
  val conVal = constant(1)
  println(conVal.headOption)
  
  //test from
  val fromVal = from(1)
  println(fromVal.headOption)
  
  //test fibo
  val fiboVal = fibs
  println(fiboVal.takeOrigin(10).toList)
  
  //test unfold
  val unfoldVal = unfold(Stream(1,2,3,4,5)) {
    case Cons(h,t) => Some((h(),t()))
    case Empty => None
  }
  println(unfoldVal.toList)
  
  //test fibs
  val fibsViaFoldVal = fibsViaUnfold
  println(fibsViaFoldVal takeOrigin 10 toList)
  
  //test fromViaUnfold
  val fromViaUnfoldVal = fromViaUnfold(1)
  println(fromViaUnfoldVal takeOrigin 10 toList)
  
  //test constantViaUnfold
  val constantViaUnfoldVal = constantViaUnfold(3)
  println(constantViaUnfoldVal takeOrigin 10 toList)
  
  //test mapViaUnfold
  val mapViaUnfoldVal = rs01.mapViaUnfold(x => x + 1)
  println(mapViaUnfoldVal takeOrigin 10 toList)
  
  //test takeViaUnfold
  println(mapViaUnfoldVal take 10 toList)
  
  //test zipWithViaFold
  val zipXs = Stream(1,2,3,4,5)
  val zipYs = Stream(6,7,8)
  println(zipXs.zipWith(zipYs){(x,y) => x + y} toList)
  
  //test zipAll
  println(zipXs zipAll zipYs toList)
  
  //test startWith
  val startWithXs = Stream(1,2,3,4,5)
  val startWithYx = Stream(1,2,3)
  println(startWithXs startsWith startWithYx)
  
  //test tail
  println(tails(startWithXs) flatMap(x => x map(x1 => x1)) toList)
  
  //test subSequence
  println(startWithXs subSequence startWithYx)
  
  //test scanRight
  val scanRightXs = Stream(1,2,3)
  val rs = scanRightXs.scanRight(0)((a,b) => a + b)
  println(rs.toList)
  
}

