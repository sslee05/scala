package basic.datastructure

object MyListTest extends App {
  val xs = MyList(1,2,3,4,5)
  
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
}

/**
 * 순수함수 자료구조로 인하여 자료구조의 공유가 가능하고
 * 그로 인하여 연산의 효율성을 보여주는 예이다.
 * 물론 init method list의 마지막을 제외한 나머지 목록을 
 * 구하는 것 같은 것은 효율이 떨어 진다.
 * Vector는 임의이 접근,갱신,head,tail,init,상수시간 요소추가를 지원한다.
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
  
  def foldRight[A,B](xs:MyList[A],z:B)(f:(A,B) => B):B = {
    xs match {
      case MyNil => z
      case MyCons(h,t) => f(h,foldRight(t,z)(f))
    }
  }
  
  def length[A](xs:MyList[A]):Int = {
    foldRight(xs,0)((x,z) => z +1)
  }
  
  @annotation.tailrec
  def foldLeft[A,B](xs:MyList[A],z:B)(f:(B,A) => B):B = {
    xs match {
      case MyNil => z
      case MyCons(h,t) => foldLeft(t,f(z,h))(f) 
    }
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
//  
//  def foldRightViaFoldLeft[A,B](xs:MyList[A])(f:(A,B) => B):B = {
//    foldLeft(reverse(xs),MyNil:MyList[A])((z,x) => f(x,z))
//  }
//  
}

sealed trait MyList[+A]
sealed case class MyCons[+A](h:A,t:MyList[A]) extends MyList[A]
object MyNil extends MyList[Nothing]