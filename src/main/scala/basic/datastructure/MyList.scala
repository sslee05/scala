package basic.datastructure

/**
 * 순수함수 자료구조로 인하여 자료구조의 공유가 가능하고
 * 그로 인하여 연산의 효율성을 보여주는 예이다.
 * 물론 init method list의 마지막을 제외한 나머지 목록을 
 * 구하는 것 같은 것은 효율이 떨어 진다.
 * Vector는 임의이 접근,갱신,head,tail,init,상수시간 요소추가를 지원한다.
 * 
 * foldLeft & foldRight 
 * foldLeft는 tail recursive 하기때문에 stack over flow 의 위험에 노출 되지 않는다. 반면 foldRight는 그러하지 못하다. 
 * function 은 수학의 공식과 같다. 
 * java의 명령어 처럼 변수에 값이 대입이 아닌 수학처럼 function 값을 전달하고 그 function의 공식의 결과에 값이 사상 된다.
 * 아래 foldRightViaLeft를 보면 foldLeft의 type B를 B => B의 function type으로 생각 함으로써 
 * lazy 실행으로 되며 foldRightViaLeft에서 호출한 foldLeft의 결과는 function (B => B) 로 가는 function 이다. 
 * 그 결과를 function(z) 로 실행함으로써 그때 공식이 평가 된다.
 */
object MyList {
  
  def apply[A](xs:A*):MyList[A] = {
    if(xs.isEmpty) MyNil
    else MyCons(xs.head,apply(xs.tail:_*))
  }
  
  
  def tail[A](xs:MyList[A]):MyList[A] = {
    xs match {
      case MyNil => MyNil
      case MyCons(x,xs1) => xs1
    }
  }
  
  def setHead[A](newHead:A,xs:MyList[A]):MyList[A] = {
    xs match {
      case MyNil => MyList(newHead)
      case MyCons(h,t) => MyCons(newHead,t) 
    }
  }
  
  def drop[A](xs:MyList[A],n:Int):MyList[A] = {
    def loop(ys:MyList[A],size:Int):MyList[A] = {
      ys match {
        case MyNil => MyNil
        case MyCons(h,t) => {
          if(size < n) loop(t,size+1)
          else t
        }
      }
    }
    
    loop(xs,0)
  }
  
  def dropWhile[A](xs:MyList[A])(p:A => Boolean):MyList[A] = {
    xs match {
      case MyNil => MyNil
      case l @ MyCons(h,t) => if(p(h)) l  else dropWhile(t)(p)
      case _ => xs
    }
  }
  
  def length[A](xs:MyList[A]):Int = {
    foldRight(xs,0)((x,z) => z +1)
  }
  
  def lengthByFoldLeft[A](xs:MyList[A]):Int = {
    foldLeft(xs,0)((z,x) => z + 1)
  }
  
  def sumByFoldLeft(xs:MyList[Int]):Int = {
    foldLeft(xs,0)((z,x) => z + x)
  }
  
  def productByFoldLeft(xs:MyList[Double]):Double = {
    foldLeft(xs,0.0)((z,x) => z + x)
  }
  
  def reverse[A](xs:MyList[A]):MyList[A] = {
    foldLeft(xs,MyNil:MyList[A])((z,x) => MyCons(x,z))
  }
  
  @annotation.tailrec
  def foldLeft[A,B](xs:MyList[A],z:B)(f:(B,A) => B):B = {
    xs match {
      case MyNil => z
      case MyCons(h,t) => foldLeft(t,f(z,h))(f) 
    }
  }
  
  
  /**
   * foldLeft는 tail recursive 하므로 stack over flow 발생하지 않는다.
   * 반면 foldRight는 그러하지 못 하다.
   * foldRight를 tail recursive하게 foldLeft를 이용하여 다시 작성하라.
   * 여기서 foldLeft에서 f(z,h)의 결과가 x:Int => Int 인 Function1 instance 임을 생각하라.
   * 
   * work flow 은 다음과 같다.f(x) = x + 1 일경우 
   * foldLeft가 실행되는 flow
   * flow 1 (1,(2::3::Nil)) :   left(2,(3::Nil),  f1(x1) = (1 + x1) )   
   * flow 2 (2,(3::Nil))    :   left(3::Nil),     f2(x2) = (2 + x2) )    x1 = 2 + x2
   * flow 3 (3::Nil)        :   left(Nil)         f3(x3) = (3 + x3) )    x2 = 3 + x3
   * fow  4 (Nil)           :   return function1 instance f3
   * result => f(x) = f1(f2(f3(x)))
   */
  def foldRightViaLeft[A,B](xs:MyList[A],z:B)(f:(A,B) => B):B = {
    foldLeft(xs,(y:B) => y)((g,a) => x => g(f(a,x)))(z)
  }
  
  /**
   * 위의 foldLeft & foldRightViaLeft를 이해하기 쉽게 다시 풀어 보면 다음과 같다.
   * 여기서의 핵심은 foldLeft의 type B를 (B => B)함수로 생각하는 것이다.
   */
  def foldLeftSolve[A,B](xs:MyList[A],z:B=>B)(f:(B=>B,A) => B => B):B => B = {
    xs match {
      case MyNil => z
      case MyCons(h,t) => foldLeftSolve(t,f(z,h))(f)
    }
  }
  
  def foldRightSolve[A,B](xs:MyList[A],z:B)(f:(A,B) => B):B = {
    val fn:B=>B = foldLeftSolve(xs,(y:B) => y)((g,a) => x => g(f(a,x)))
    fn(z)
  }
  
  def foldRight[A,B](xs:MyList[A],z:B)(f:(A,B) => B):B = {
    xs match {
      case MyNil => z
      case MyCons(h,t) => f(h,foldRight(t,z)(f))
    }
  }
  
  /**
   * flow 1 (1,(2::3::Nil) ) :  right.f(1,right((2::3::Nil,  f1(x1) = x1) )  g1(x1) = x1 + 1 : 
   * flow 2 (2,(3::Nil)      :  right.f(2,right(3::Nil,      f1(x1) = x1) )  g2(x2) = x2 + 2 : x2 = g1(x1)
   * flow 3 (Nil)            :  right.f(3,right(Nil,         f1(x1) = x1) )  g3(x1) = x1 + 3 : x1 = g2(x2)
   * flow 4                  :  return f1(x1) = x1
   * result => g(x) = g3(g2(g1(x)))
   */
  def foldLeftViaRight[A,B](xs:MyList[A],z:B)(f:(B,A) => B):B = {
    foldRight(xs,(y:B) => y)((a,g) => x => g(f(x,a)))(z) 
  }

}

sealed trait MyList[+A]
sealed case class MyCons[+A](h:A,t:MyList[A]) extends MyList[A]
object MyNil extends MyList[Nothing]


object MyListTest extends App {
  val xs = MyList(1,2,3,4,5)
  
  val f01 = (x:Int) => x
  val f02 = (x:Int) => x + 1
  
  println(f01(f02(2)))
  
  val result:MyList[Int] = MyList.tail(xs)
  println(result)
  
  val result2 = MyList.setHead(6, xs)
  println(result2)
  
  val result3 = MyList.drop(xs,2)
  println(result3)
  
  val result4 = MyList.dropWhile(xs)(_ > 3)
  println(result4)
  
  val result5 = MyList.foldRight(MyList(1,2,3),MyNil:MyList[Int])(MyCons(_,_))
  println(result5)
  
  val result6 = MyList length xs
  println(result6)
  
  val result7 = MyList lengthByFoldLeft xs
  println(result7)
  
  val result8 = MyList sumByFoldLeft xs
  println(result8)
  
  val result9 = MyList productByFoldLeft MyList(0.1,0.2,3.0,4.0,5.0)
  println(result9)
  
  val result10 = MyList reverse xs
  println(result10)
  
  
  def test02[A,B](a:A,b:B)(f:(B,A) => B) = f(b,a)
  def test03[A,B](a:A,b:B)(f:(A,B) => B):B = {
    test02(a,(b:B) => b)((g,a) => z => g(f(a,z)))(b:B)
  }
  
}