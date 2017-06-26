package basic

/**
 * 참조투명성 검증 치환모형
 */
object RT extends App {
  
  /**
   * 참조투명 
   */
  val x1 = "Hello,World"
  val r1 = x1.reverse
  val r2 = x1.reverse
  println(r1)//dlroW,olleH
  println(r2)//dlroW,olleH
  
  //x1 표현식을 "Hello,World" 로 치환
  val r3 = "Hello,World".reverse
  println(r3)//dlroW,olleH
  val r4 = "Hello,World".reverse
  println(r4)//dlroW,olleH

  /**
   * 참조투명하지 않음  
   */
  val x2 = new StringBuilder("Hello")
  val y = x2.append(",World")
  
  val r5 = y.toString()
  val r6 = y.toString();
  
  
  //y의 표현식을 x2.append(",Wold") 로 대치한다.
  val r7 = x2.append(",World").toString();
  val r8 = x2.append(",World").toString();
  
  println(r7)//Hello,World,World
  println(r8)//Hello,World,World,World
  
  
}