package exercise.laziness.repeat02

import exercise.laziness.repeat02.Stream._

//ex-01) Stream trait 와 그를 상속하는 Cons와 Empty case class를 작성하되.
//Cons의 class parameter를 laziness 하게 작성하라.
trait Stream[+A] {
  
  //ex-06) Stream을 List로 반환하는 함수를 작성하라.
	def toList: List[A] = {
	  
	  @annotation.tailrec
	  def go(xs: Stream[A],rs: List[A]): List[A] = xs match {
	    case Empty => rs.reverse
	    case Cons(h,t) => go(t(),h()::rs)
	  }
	    
	  go(this,List[A]())
	}
	
  def toListNotReverse: List[A] = {
    type B = List[A] => List[A]
    
    @annotation.tailrec
    def go(xs: Stream[A])(z: B)(f: (B,A) => B): B = xs match {
      case Empty => z
      case Cons(h,t) => go(t())(f(z,h()))(f)
    }
    
    go(this)((y:List[A]) => y)((g,a) => b => g(a :: b))(List[A]()) 
  }
  
  def toListFast: List[A] = {
    val bufList = new scala.collection.mutable.ListBuffer[A]
    
    @annotation.tailrec
    def go(xs: Stream[A]): List[A] = xs match {
      case Empty => bufList.toList
      case Cons(h,t) => {
        bufList += h()
        go(t())
      }
    }
    
    go(this)
  }
  
  //ex-07) Stream의 처음 n개의 요소를 돌려주는 함수를 작성하라
  def takeOrigin(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => cons(h(),t().takeOrigin(n-1))
    case _ => empty[A]
  }
  
  //ex-08) Stream에서 처음 n개의 요소를 건너뛰고 반환하는 함수를 작성하라.
  def dropOrigin(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => t() dropOrigin (n-1)
    case _ => this
  }
  
  //ex-09) 주어진 술어를 만족하는 선행 요소들을 모두 돌려주는 함수를 작성하라.
  def takeWhileOrigin(p: A => Boolean): Stream[A] = this match {
    case Empty => empty[A]
    case Cons(h,t) if p(h()) => cons(h(),t() takeWhileOrigin p)
    case Cons(h,t) => t() takeWhileOrigin p
  }
  
  //ex-10) 술어를 만족하는 원소가 있는지 판별하는 exists 를 구현하라.
  def exists(p: A => Boolean): Boolean = this match {
    case Empty => false
    case Cons(h,t) => p(h()) || (t() exists p)
  }
  
  //ex-11) foldRight를 구현하되 2번째 누적 인자를 laziness 하게 하라.
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h,t) => f(h(),t().foldRight(z)(f))
  }
  //def foldRight[B](z: => B)(f: (A, => B) => B): B = ???
  
  //ex-12) foldRight를 이용하여 Stream의 모든 요소가 주어진 술어를 모두 만족하는지 판별하는 함수를 작성하라. 만족하지 않으면 즉시 순회를 마처야 한다.
  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a,b) => p(a) && b)
    
  //ex-13) foldRight를 이용하여 takeWhile를 구현하라.
  def takeWhile(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((a,b) => if(p(a)) cons(a,b) else b)
    
  //ex-14) foldRight를 이용하여 headOption을 구현하라.
  def headOption: Option[A] = 
    foldRight[Option[A]](None)((a,b) => Some(a))
   
  //ex-15) foldRight를 이용하여 map 함수를 구현하라.
  def map[B](f: A => B): Stream[B] = 
    foldRight(empty[B])((a,b) => cons(f(a),b))
  
  //ex-16) foldRight를 이용하여 append 함수를 구현하라. 자신의 인수에 대하여 엄격하지 않아야 한다.
  def append[B >: A](xs: => Stream[B]): Stream[B] = 
    foldRight(xs)((a,b) => cons(a,b))
    
  //ex-17) foldRight를 이용하여 flatMap함수를 구현하라.
  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty[B])((a,b) => f(a) append b)
    
  //ex-18) filter를 foldRight를 이용하여 구현하라.
  def filter(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((a,b) => if(p(a)) cons(a,b) else b)
    
  //ex-26)unfold를 이용하여 map을 구현하라.
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this){
    case Empty => None
    case Cons(h,t) => Some((f(h()), t()))
  }
  
  //ex-27)unfold를 이용하여 take를 구현하라.
  def takeViaUnfold(n: Int): Stream[A] = unfold((this,n)) {
    case (Cons(h,t),x) if x > 0 => Some((h(),(t(),x-1))) 
    case _ => None
  }
  
  
  //ex-28)unfold를 이용하여 takeWhile를 구현하라.
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this){
    case (Cons(h,t)) if p(h()) => Some((h(),t()))
    case _ => None
  }
  
  //ex-29)unfold를 이용하여 zipWith를 구현하라.
  def zipWith[B,C](sx: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this,sx)) {
    case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(),h2()),(t1(),t2())))
    case _ => None
  }
  
  //ex-30)unfold를 이용하여 zipApp를 구현하라 Stream 요소가 더 있는한 순회를 계속해야 한다.
  def zipAll[B](sx: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this,sx)) {
    case (Cons(h1,t1),Cons(h2,t2)) => Some(( (Some(h1()), Some(h2()))  , (t1(),t2()) ))
    case (Cons(h1,t1), Empty) => Some(( (Some(h1()),None) , (t1(),empty[B]) ))
    case (Empty, Cons(h2,t2)) => Some(( (None,Some(h2())) , (empty[A],t2()) ))
    case _ => None
  }
  
  //ex-33) subSequence를 작성하라.
  def subSequence[B >: A](sub:Stream[B]): Boolean = 
    sub.foldRight(true)((a,b) => this.exists(x => x == a) && b)
  
  //ex-32)위의 function 들로 아래와 같은 것을 구현하라.
  //Stream(1,2,3,4,5) 와 Stream(1,2,3) 은 true Stream(2,3,4) 는 false
  def startsWith[B >: A](sub:Stream[B]): Boolean = 
    zipAll(sub).filter(a => a._2 != None).forAll(a => a._1.get == a._2.get)
  
  //ex-33) unfold 를 이용하여 다음을 구성하라.
  //Stream(1,2,3) 에 대하여 
  //Stream(Stream(1,2,3),Stream(2,3),Stream(3),Stream()) 을 반환 해야 한다.
  def tails:Stream[Stream[A]] = unfold(this){
    case Cons(h,t) => Some(( cons(h(),t()) , t()))
    case _ => None
  }append empty
  
  //ex-34) tail를 이용한 subSequence를 재구현 하라.
  def subSequenceViaTails[B >: A](sx: Stream[B]): Boolean = 
    tails.exists(s => s startsWith sx)
  
  /*
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z,Stream(z)))( (a,b) => {
      lazy val p1 = b
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
  */
}

// class parameter의 laziness 는 기술적 한계로 인하여 명시적 강제 표현을 해야 한다.
sealed case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]
object Empty extends Stream[Nothing]

//ex-02 Stream object를 생성하라.
object Stream {
  
  //ex-03) cons 의 smart 생성자를 작성하라.
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }
  
  //ex-04) Empty 을 반환하는 함수를 작성하라.
  def empty[A]: Stream[A] = Empty
  
  //ex-05) apply 적용함수를 작성하라.
  //A * 은 Seq type 이다.
  def apply[A](xs: A*): Stream[A] = {
    if(xs.isEmpty) empty[A]
    else cons(xs.head,apply(xs.tail: _*))
  }
    
  //ex-19) 주어진 값의 무한 Stream을 돌려주는 함수 constant를 구현하라.
  def constant[A](a: A): Stream[A] = cons(a,constant(a))
    
  //ex-20) n으로 시작해서 n+1, n+2, n+3 .... 으로 하는 무한 Stream을 생성하는 함수를 작성하라.
  def from(n: Int): Stream[Int] = cons(n,from(n+1))
    
  //ex-21) 0,1,1,2,3,5,8...피보나치 무한 수열로 이루어진 함수를 작성하라.
  def fibo(pre: Int, cur: Int): Stream[Int] = cons(cur,fibo(cur, pre + cur))
    
  //ex-22) 초기상태 하나와 다음 상태 및 다음 값(생성된 stream 안의)을 산출하는 함수 하나를 받는 Stream 구축 함수를 구현하라.
  //이를 공재귀(corecursive) 라 한다. 재귀함수는 자료를 소비하지만 공재귀는 생산한다.
  //재귀는 점점 더 작은 입력으로 재귀하다가 종료되지만, 공재귀 함수는 생산성을 유지하는 한 종료될 필요는 없다.
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a,s)) => cons(a,unfold(s)(f))
  }
  
  //ex-23)unfold를 이용하여 fibs를 재구현하라.
  def fibsViaUnfold: Stream[Int] = unfold((0,1)) {
    case (pre,cur) => Some((pre,(cur,pre + cur)))
  }
    
  //ex-24)unfold를 이용하여 from을 재구현하라.
  def fromViaUnfold(n: Int): Stream[Int] = unfold(n){case x => Some((x, x+1))}
    
  //ex-25)unfold를 이용하여 ones를 구현하라.
  def ones: Stream[Int] = unfold(1){case x => Some((x,x))}
  
}

object StreamDriver extends App {
  val xs01 = Stream(1,2,3,4,5)
  println(xs01.toList)
  println(xs01.toListNotReverse)
  println(xs01.toListFast)
  
  println(xs01.takeOrigin(2).toList)
  println(xs01.dropOrigin(2).toList)
  //fibsViaUnfold
  println(fibsViaUnfold.takeOrigin(8).toList)
  
  
  println(xs01.takeViaUnfold(3).toList)
  
  val xs02 = Stream(2,3)
  println(xs01.subSequence(xs02))
  
  val xs03 = Stream(1,2)
  println(xs01 startsWith xs02)
  println(xs01 startsWith xs03)
  
  val xs04 = Stream(1,2,3)
  println(xs04.tails.toList)
  
  println(xs01 subSequenceViaTails xs04)
}