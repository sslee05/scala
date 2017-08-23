package basic.errorhandling

/**
 * Option 과 같지만 exception 정보를 담을 수 있다.
 * 이또한 ADT 구조형의 container type 이다.
 */
sealed trait MyEither[+E,+A] {
  
  def map[B](f:A => B):MyEither[E,B] = {
    this match {
      case MyLeft(e) => MyLeft(e)
      case MyRight(x) => MyRight(f(x))
    }
  }
  
  def flatMap[EE>:E,B](f:A => MyEither[EE,B]):MyEither[EE,B] = {
    this match {
      case MyLeft(e) => MyLeft(e)
      case MyRight(x) => f(x)
    }
  }
  
  def orElse[EE>:E,B>:A](b: => MyEither[EE,B]):MyEither[EE,B] = {
    this match {
      case MyLeft(e) => b
      case MyRight(x) => MyRight(x)
    }
  }
  
  def map2[EE>:E,B,C](b:MyEither[EE,B])(f:(A,B) => C):MyEither[EE,C] = {
    flatMap(a => b map(b1 => f(a,b1)))
  }
  
}

sealed case class MyRight[+A](value:A) extends MyEither[Nothing,A]
sealed case class MyLeft[+E](e:E) extends MyEither[E,Nothing]

object MyEither {
  def sequence[E,A](xs:List[MyEither[E,A]]):MyEither[E,List[A]] = {
    xs.foldRight[MyEither[E,List[A]]](MyRight(Nil))((x,g) => g.map2(x)((g1,x1) => x1::g1))
  }
  
  def traverse[E,A,B](xs:List[A])(f:A => MyEither[E,B]):MyEither[E,List[B]] = {
    //sequence(xs map f)
    xs.foldRight[MyEither[E,List[B]]](MyRight(Nil))((x,g) => g.map2(f(x))((g1,x1) => x1::g1 ))
  }
  
  def sequenceViaTraverse[E,A](xs:List[MyEither[E,A]]):MyEither[E,List[A]] = {
    traverse(xs)(x => x)
  }
}
