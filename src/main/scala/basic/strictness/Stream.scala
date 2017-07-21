package basic.strictness
import basic.strictness.Stream._

sealed trait Stream[+A] {
  
  def toList:List[A] = {
    @annotation.tailrec
    def go[B](xs:Stream[A],z:B)(f:(A,B) => B): B = {
      xs match {
        case Empty => z
        case Cons(h,t) => go(t(),f(h(),z))(f)
      }
    }
    
    go(this,(y:List[A]) => y)((a,g) => x => g(a::x))(Nil:List[A])
  }
  
  def take(n:Int):Stream[A] = {
    this match {
      case Empty => empty
      case Cons(h,t) if(n > 0) => cons(h(),t().take(n-1))
      case Cons(h,_) if(n == 1) => cons(h(),empty)
      case _ if n <= 0 => empty
    }
  }
  
  def drop(n:Int):Stream[A] = {
    this match {
      case Empty => empty
      case Cons(h,t) if(n > 0) => t().take(n-1)
      case _ => this
    }
  }
  
  def takeWhile(p:A => Boolean):Stream[A] = {
    this match {
      case Empty => empty
      case Cons(h,t) if p(h()) => cons(h(),t() takeWhile p)
      case Cons(h,t) if !p(h()) => t() takeWhile p
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
    foldRight(false)((h,t) => p(h) || t)
  
  def forAll(p:A => Boolean):Boolean =
    foldRight(true)((h,t) => p(h) && t)
  
  def takeWhileVaFold(p:A => Boolean):Stream[A] = 
    foldRight(empty[A])((h,t) => if(p(h)) cons(h,t) else t)  
  
  def headOption:Option[A] = 
    foldRight(None:Option[A])((h,_) => Some(h)) 
    
  def map[B](f:A => B):Stream[B] = 
    foldRight(empty[B])((h,t) => cons(f(h),t))
  
  def append[B >: A](xs: => Stream[B]):Stream[B] = 
    foldRight(xs)((a,b) => cons(a,b))
    
  def flatMap[B](f:A => Stream[B]):Stream[B] =
    foldRight(empty[B])((a,b) => f(a) append b)
    
  def filter(p:A => Boolean):Stream[A] = 
    foldRight(empty[A])((a,b) => if(p(a)) cons(a,b) else b)
}

/**
 * class parameter 인 경우 기술적인 문제로 인하여 이름으로 전달되는 인수가 아니라 
 * 반드시 명시적으로 강제해야 하는 thunk 이어야 한다
 */
sealed case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Empty extends Stream[Nothing]


object Stream {
  def cons[A](h: => A, t: => Stream[A]):Stream[A] = {
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
  
  def ones:Stream[Int] = cons(1,ones)
  
  def constant[A](a:A):Stream[A] = {
    lazy val tail:Stream[A] = Cons(() => a,() => tail)
    tail
  }
  
  def constant02[A](a:A):Stream[A] = {
    cons(a,constant02(a))
  }
  
  /**
   * n에서 시작해서 n+1,n+2 이어지는 무한정수 stream을
   * 생성하는 함수를 작성하라.
   */
  def from(n:Int):Stream[Int] ={
    cons(n,from(n+1))
  }
  
  /**
   * fibo 수 0,1,1,2,3,5,8,13 ... 로 이루어진
   * 무한 stream를 생성하는 fibs를 작성하
   */
  def fibs:Stream[Int] = {
    def go(cur:Int,pre:Int):Stream[Int] = {
      cons(pre,go(pre + cur,cur))
    }
    
    go(1,0)
  }
  
  /**
   * 일반화된 Stream 구축 함수를 작성 하라.
   * 이 함수는 초기상태 하나와 다음 상태 및 다음 값(생성된 Stream 안의)를 
   * 산출하는 함수 하나를 받아야 한다.
   */
  def unfold[A,S](z:S)(f:S => Option[(A,S)]):Stream[A] = {
    f(z) match {
      case Some((h,t)) => cons(h,unfold(t)(f))
      case None => empty
    }
  }
  
  /**
   * unfold를 이용하여 fibs 를 재정의 하라.
   */
  def fibsViaUnfold:Stream[Int] = 
    unfold((0,1)){ case (x,y) => Some((x,(y,x+y)))}
  
  /**
   * unfold를 이용하여 from를 재정의 하라.
   */
  def fromViaUnfold(n:Int):Stream[Int] = 
    unfold(1){ x => Some((x,x + 1))}
  
  /**
   * unfold를 이용하여 constant를 재정의 하라.
   */
  def constantViaUnfold(n:Int):Stream[Int] = 
    unfold(n){ x => Some(x,x) }
  
  def onesViaUnfold:Stream[Int] =
    unfold(1){ x => Some(x,x) }
  
  /**
   * unfold를 이용해서 map를 재정의 하라.
   */
  def mapViaUnfold[A,B](xs:Stream[A])(f:A => B):Stream[B] = 
    unfold(xs) { 
      case Cons(h,t) => Some((f(h()),t()))
      case _ => None
    }
  
  def takeViaUnfold[A](xs:Stream[A])(n:Int):Stream[A] = 
    unfold((xs,n)){  
      case (Cons(h,t),x) if(x > 0) => Some((h(),(t(),x-1)))
      case _ => None
    }
  
  def takeWhileViaUnfold[A](xs:Stream[A])(p:A => Boolean):Stream[A] = 
    unfold(xs){
      case Cons(h,t) if p(h()) => Some( (h(),t()) )
      case _ => None
  }
}

object StreamDriver extends App {


  // && ,|| 은 비엄격적 이다.
  //non-strictness 의 표현방법과 평가,호출의 2가지 방법.
  println(false && { println("is called"); true })
  println(true || { println("is called"); false })

  //1. 이름으로 표현 (literal function 이라 생각하면 됨)
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A = {
    if (cond) onTrue else onFalse
  }

  //2.thunk 으로 표현(Funtion0 이라 생각 하면 됨)
  def if3(cond: Boolean, onTrue: () => Int, onFalse: () => Int): Int = {
    if (cond) onTrue() else onFalse()
  }

  println(if2(true, { println("called onTrue"); 1 }, { println("called onFalse"); 2 }))
  println(if3(true, () => { println("called onTrue2"); 1 }, () => { println("called onFalse2"); 2 }))

  //### cache
  def mayBeTwice(b: Boolean, i: => Int): Int = if (b) i + i else 1

  def mayBeTwiceCache(b: Boolean, i: => Int): Int = {
    lazy val j = i
    if (b) j + j else 1
  }

  println(mayBeTwice(true, { println("called"); 2 }))
  println(mayBeTwiceCache(true, { println("called"); 3 }))
  
  val xs = Stream(1,2,3,4,5)
  println(xs)
  val rs = xs.toList
  println(xs.takeWhileVaFold(_ > 4).headOption)
  
  val rs02 = xs.map(x => x * 2).toList
  println(rs02)
  
  val ys = Stream(6,7,8,9,10)
  println(xs append ys toList)
  
  val xs03 = Stream[Stream[Int]](Stream(1),Stream(2),Stream(4))
  val rs03 = xs03.flatMap(x => x.map(x1 => x1 * 2)).toList
  println(rs03)
  
  println(xs filter(_ > 4) headOption)
  
  //##############################################
  //############### infinite #####################
  //##############################################
  val rs04 = ones.take(5)
  println(ones.take(5).toList)
  println(ones exist (x => x % 2 != 0) )
  println(ones.map(x => x + 1).exist(x => x % 2 == 0))
  
  println(constant[Int](1))
  println(constant02[Int](1))
  
  val test01 = constant[Int](1)
  val test02 = constant02[Int](1)
  println(test01.take(5).toList)
  println(test02.take(5).toList)
  
  val test03 = from(1)
  println(test03.take(5).toList)
  
  val xs04 = fibsViaUnfold
  println(xs04.take(8).toList)
  
  val xs05 = fromViaUnfold(5)
  println(xs05.take(10).toList)
  
  val xs06 = mapViaUnfold(xs04)(x => x + 1)
  println(xs06.take(10).toList)
  
  val xs07 = takeViaUnfold(xs05)(3)
  println(xs07.toList)
  
  val xs08 = takeWhileViaUnfold(xs05)(x => x < 13)
  println(xs08.toList)
}