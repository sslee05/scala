package basic

object PartiallyAppliedFunction extends App {
  
  //parameter 일부 인자 partially applied function 적용 
  def testFn(s1:String,s2:String) = s1 + s2;
  val partFn01 = testFn("Hellow ",_ : String)
  println(partFn01("scala"))
  println(partFn01.apply("scala"))
  
  
  //여러 인자 목록 (currying)
  def testFn02(s1:String)(s2:String) = s1 + s2
  val partFn02 = testFn02("Hellow ") _
  println(partFn02("scala"))
  println(partFn02.apply("scala"))
  
  
  def modF(a:Int)(b:Int) = b % a == 0
  val xs = List(1,2,3,4,5,6,7,8,9,10)
  
  def filter(xs:List[Int])(p:Int => Boolean):List[Int] = {
    xs match {
      case Nil => Nil
      case(x::xs1) => if(p(x)) x::filter(xs1)(p) else filter(xs1)(p)
    }
  }
  
  // filter의 두번째 인자는 Int => Boolean 유형의 function을 받는 것이 명확하므로 modF(2) _ 을 
  // 넘기는 것이 아닌 _를 생략한 modF(2)를 넘길 수 있다.
  println(filter(xs)(modF(2)))
  
}