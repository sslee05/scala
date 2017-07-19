package basic.strictness
import basic.strictness.Stream._

trait Stream[+A] {

  def headOption: Option[A] = {
    this match {
      case Empty      => None
      //evaluation 됨. Cons의 class parameter가 thunk 이므로 평가해야 함.
      case Cons(h, t) => Some(h()) 
    }
  }

  def toList: List[A] = {

    @annotation.tailrec
    def go(xs: Stream[A],z:List[A] => List[A])(f:(A, List[A] => List[A]) => List[A] => List[A]):List[A] =>List[A] = {
      xs match {
        case Empty      => z
        case Cons(h, t) => go(t(),f(h(),z))(f)
      }
    }

    go(this, (y: List[A]) => y)((a, g) => x => g(a :: x))(Nil: List[A])
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Empty => Stream.empty
      case Cons(h, t) if n > 1 =>
        //위의 headOption의 Some(h()) 부분과 다르게  
        //h(), t() 은 호출시 강제평가 표현이 있더라도 cons(h: => A,t: => Stream[A]) 
        //이기 때문에 호출시 평가 되는 것이 아니다.
        //이는 scala compiler 가  () => h() thunk 으로 감싼다.
        cons(h(), t().take(n - 1)) 
      case Cons(h,_) if n == 1 => cons(h(),Stream.empty)  
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, t) if n >= 1 => t().take(n - 1)
      case _                    => this
    }
  }

  def dropWhile(p: A => Boolean): Stream[A] = {
    this match {
      //p(h()) 이때 head는 평가 되나 cons(h(),t()) 의 h() 는 scala가 thunk으로 감싸기 때문에 
      //평가가 미루어 진다.
      case Cons(h, t) if p(h()) => cons(h(), t() dropWhile p) 
      case _ => Empty
    }
  }
  
  def takeWhile(p: A => Boolean):Stream[A] = {
    this match {
      case Cons(h,t) => if(p(h())) cons(h(),t() takeWhile p) else t() takeWhile p
      case Empty => Stream.empty
    }
  }

  /**
   * foldRight에 A 부분은 call-by-value, B	부분은 call-by-name 이다. 
   * 따라서 A부분만으로 결과를 return 된다면 B부분은(B은 thunk상태로 들어가 있다) 실행 되지(thunk의 평가) 않아도 된다.
   */
  def foldRight[B](z: => B)(f:(A, => B) => B):B = {
    this match {
      case Empty     => z
      case Cons(h,t) => f(h(),t().foldRight(z)(f))
    }
  }
  
  /**
   * 위의 foldRight를 이용한 것으로 header가 predicate를 만족하면 
   * 이후는 평가되지 않고 작업를 완료하게 된다. 
   */
  def exists(p:A => Boolean):Boolean = foldRight(false)((h,t) => p(h) || t)
  
  /**
   * 모든 원소가 predicate를 만족하는지 check는 = 1개라도 만족 하지 않으면 false 
   * 따라서 header가 1먼저 false를 하면 이후는 평가 되지 않는다.
   */
  def forAll(p:A => Boolean):Boolean = foldRight(true)((h,t) => p(h) && t)
  
  def takeWhileViaFold(p:A => Boolean):Stream[A] = {
    foldRight(empty[A])((h,t) => if(p(h)) cons(h, t) else t)
  }
  
  def headOptionViaFoldRight:Option[A] = foldRight(None:Option[A])((h,_) => Some(h))
  
  def map[B](f:A => B):Stream[B] = 
    foldRight(empty[B])((h,t) => cons(f(h),t))
    
  def append[B >: A](xs:Stream[B]):Stream[B] = 
    foldRight(xs)((h,t) => cons(h,t))
    
  def flatMap[B](f:A => Stream[B]):Stream[B] = 
    foldRight(empty[B])((a,b) => f(a) append(b) )
    
  def filter(p:A => Boolean):Stream[A] = 
    foldRight(empty[A])((h,t) => if(p(h)) cons(h,t) else t)
    
}

//Cons[+A](h: => A,t: => Stream[A]) 둘다 non-strictness 이지만
//class parameter 인 경우 기술적인 문제로 인하여 이름으로 전달되는 인수가 아니라 반드시 명시적으로 강제해야 하는 thunk 이어야 한다.
sealed case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
case object Empty extends Stream[Nothing]

object Stream {

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))
  }

}

object StreamDriver extends App {
  
  val xs = Stream(1, 2, 3, 4, 5)
  val rs = xs.takeWhileViaFold(x => x > 2)
  println(rs)
  
  // && ,|| 은 비엄격적 이다.
  //non-strictness 의 표현방법과 평가,호출의 2가지 방법.
  println(false && {println("is called");true})
  println(true || {println("is called"); false})
  
  
  //1. 이름으로 표현 (literal function 이라 생각하면 됨)
  def if2[A](cond:Boolean,onTrue: => A,onFalse: => A):A = {
    if(cond) onTrue else onFalse
  }
  
  //2.thunk 으로 표현(Funtion0 이라 생각 하면 됨)
  def if3(cond:Boolean,onTrue:() => Int,onFalse:() => Int):Int = {
    if(cond) onTrue() else onFalse()
  }
  
  println(if2(true,{println("called onTrue");1},{println("called onFalse");2}))
  println(if3(true,() => {println("called onTrue2");1},() => {println("called onFalse2");2}))
  
  //### cache
  def mayBeTwice(b:Boolean,i: => Int):Int = if(b) i + i else 1
  
  def mayBeTwiceCache(b:Boolean,i: => Int):Int = {
    lazy val j = i
    if(b) j + j else 1
  }
  
  println(mayBeTwice(true,{println("called");2}))
  println(mayBeTwiceCache(true,{println("called");3}))
 
}