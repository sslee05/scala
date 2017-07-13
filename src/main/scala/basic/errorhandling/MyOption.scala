package basic.errorhandling

/**
 * 순수함수는 부수효과가 없고 참조투명하다
 * 참조투명함은 문맥에 의존하지 않는다.
 * exception은 문맥의 위치에 따라 결과가 달라지는 의존적이다.
 * scala는 오류에 미리정의해 하는 container를 제공하며 이는 
 * 고차함수의 기능으로 캡슐화 되어있다. 
 */
sealed trait MyOption[+A] {
  
  /**
   * Option의 값을 얻는 getOrElse를 구현하라.
   */
  def getOrElse[B >: A](default: => B):B = {
    this match {
      case MyNone => default
      case MySome(x) => x
    }
  }
  
  /**
   * map 를 구현 하라.
   */
  def map[B](f:A => B):MyOption[B] = {
    this match {
      case MyNone => MyNone
      case MySome(x) => MySome(f(x))
    }
  }
  
  /**
   * pattern match 를 이용하지 말고 flatMap를 구현하라.
   */
  def flatMap[B](f:A => MyOption[B]):MyOption[B] = {
    map(f) getOrElse MyNone
  }
  
  /**
   * MyOption을 구하는 함수를 구현하라. 
   */
  def orElse[B >: A](obj: => MyOption[B]):MyOption[B] = {
    map(x => MySome(x)) getOrElse obj
  }
  
  /**
   * 술어만족에 따른  MyOption를 구하는 함수를 작성하라. 
   */
  def filter(p:A => Boolean):MyOption[A] = {
    flatMap(x => if(p(x)) MySome(x) else MyNone)
  }
  
  /**
   * lift 를 구현하여 raw type 을 Option으로 처리하는 승격 함수를 구현하라.
   */
  def map2[B,C](b:MyOption[B])(f:(A,B) => C):MyOption[C] = {
    flatMap(a => b.map(b1 => f(a,b1)))
  }
  
}

object MyOption {
  def sequence[A](xs:List[MyOption[A]]):MyOption[List[A]] = {
    xs.foldRight[MyOption[List[A]]](MySome(Nil:List[A]))((x,y) => y.map2(x)((y,x) => x::y) )
  }
  
  def traverse[A,B](xs:List[A])(f:A => MyOption[B]):MyOption[List[B]] = {
    //sequence(xs map(x => f(x)))  2번 순회하여 비효율적 이다.
    xs.foldRight[MyOption[List[B]]](MySome(Nil:List[B]))((x,y) => y.map2(f(x))((y1,x1) => x1::y1) )
  }
  
  def sequenceViaTraverse[A](xs:List[MyOption[A]]):MyOption[List[A]] = {
    traverse(xs)(x => x)
  }
}

sealed case class MySome[+A](value:A) extends MyOption[A]
object MyNone extends MyOption[Nothing]

