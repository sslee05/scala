package basic.monoid

object study {
  println("Welcome to the Scala worksheet")
  
  //monoid law = 항등원의 법칙, 결합법칙
  //monoid 구성
  // 어떤 형식 A
  // 그 형식 2개를 하나의 값을 산출하는 항등법칙과 결합법칙이 성립하는 이항연산
  // 항등원
  trait Monoid[A] {
    
    //op(op(a1,a2),a3) == op(a1,op(a2,a3))
    def op(a1: A, a2:A): A = ???
    
    //op(a1,zero) == op(zero,a1) == a1
    def zero: A = ???
  }
  
  //String monoid
  val stringMonoid = new Monoid[String] {
    def op(s1: String, s2: String): String = s1 + s2
    def zero: String = ""
  }
}