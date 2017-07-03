package basic

object ViewBoundTest extends App {
  
  val xs01 = List(1,2,3,10,9,8,7,6,5,4)
  val xs02 = List(MyOrdered(1),MyOrdered(2),MyOrdered(5),MyOrdered(4),MyOrdered(7))
  //val viewBoundTest:ViewBoundTest = new ViewBoundTest()
  println(ViewBoundObj maxValue01 xs02)
  println(ViewBoundObj maxValue02 xs01)
  println(ViewBoundObj maxValue03 xs01)
  println(ViewBoundObj maxValueWithViewBound xs01)

}

case class MyOrdered(value:Int) extends Ordered[MyOrdered] {
  def compare(that:MyOrdered):Int = {
    this.value - that.value
  }
}

object ViewBoundObj {
  
  /**
   * 이는 List의 원소 type T는 Ordered[T]의 하위 type 인 것 만 사용 할 수 있다.
   * 따라서 List(Int) 은 사용할 수 없다. Int 은 Ordered[Int] 의 하위 type이 아니므로.
   */
  def maxValue01[T <: Ordered[T]](xs:List[T]):T = {
    xs match {
      case Nil => throw new RuntimeException("must not be empty")
      case List(x) => x
      case (x::xs1) => {
        val maxV = maxValue01(xs1)
        if(x > maxV) x else maxV
      }
    }
  }
  
  /**
   * Ordered를 암시적 변환에 적용함으로써 일반적인 type 사용가능하게 되었다.
   */
  def maxValue02[T](xs:List[T])(implicit ordered:T => Ordered[T]):T = {
    xs match {
      case Nil => throw new RuntimeException("must not be empty")
      case List(x) => x
      case (x::xs1) => {
        val xsV = maxValue02(xs1)// parameter 목록 암시적변환 적용됨 maxValue02(xs1)(ordered)
        if(ordered(x) > xsV) x else xsV  
      }
    }
  }
  
  /**
   * parameter호출 암시적 변환선으로 method내의 호출대상 암시적 변환까지 적용 되었다.
   */
  def maxValue03[T](xs:List[T])(implicit ordered:T => Ordered[T]):T = {
    xs match {
      case Nil => throw new RuntimeException("must not be empty")
      case List(x) => x
      case (x::xs1) => {
        val xsV = maxValue03(xs1)// parameter 목록 암시적변환 적용됨 maxValue03(xs1)(ordered)
        if(x > xsV) x else xsV //호출대상 객체 암시적변환 적용됨 ordered(x) > xsV 
      }
    }
  }
  
  /**
   * 상위 bound( <% )를 적용해서 위의 maxValue03과 같은 결과를 도출 해냄.
   * 상위 bound는 T 가 Ordered[T]의 하위 type이란 의미가 아닌 즉 Ordered[T]의 subType과의 관계가 아닌 T가 
   * Ordered[T]을 지원할 수 있는 유형을 뜻 함.
   * Int 는 Ordered[Int] 의 하위 type이 아니지만 Int => Ordered[Int] 로 변화하는 암시적 값이 있으면 가능하다.
   */
  def maxValueWithViewBound[T <% Ordered[T]](xs:List[T]):T = {
    xs match {
      case Nil => throw new RuntimeException("must not be empty")
      case List(x) => x
      case (x::xs1) => {
        val xsV = maxValueWithViewBound(xs1)
        if(x > xsV) x else xsV
      }
    }
  }
}


