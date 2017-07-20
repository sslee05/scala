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

}