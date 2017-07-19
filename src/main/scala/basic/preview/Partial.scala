package basic.preview

object Partial extends App {
  
  //case literal 은 부분함수(partial function) 이다.
  //partialFn3은 type이 List[Int] => Int 인 함수 유형이다(부분함수를 포함한)
  //따라서 partialFn3은 isDefinedAt 를 만들어 주지 않는다.
  //유형을 PartialFunction[List[Int],Int] 로 선언하면 isDefinedAt이 만들어 진다.
  //compiler는 type이 Function1 이거나 type표기가 없다면 함수 literal를 완전함수로 변환한다.
  val partialFn3:List[Int] => Int = {
    case x::y::_ => x
  }
  
  //partialFn3.isDefinedAt(List(1,2,3)) 이는 compile error
  val partialFn:PartialFunction[List[Int],Int] = {
    case x::y::_ => y
  }
  
  //부분함수(partial function)인 경우 2가지 function
  //을 제공해준다. apply,isDefinedAt
  println(partialFn.isDefinedAt(List(1,2,3)))
  
  //partialFn은 partial function 이며 이는 아래와 같은 code가 생성됨.
  val partialFn2 = new PartialFunction[List[Int],Int] {
    def apply(xs:List[Int]) = xs match {
      case x::y::_ => y
    }
    
    def isDefinedAt(xs:List[Int]) = xs match {
      case x::y::_ => true
      case _ => false
    }
  }
  
  
  //collect 와 map function의 차이
  var rs = List(1,2,3,4,5,"aa") collect({case i:Int => i + 1})
  rs.foreach(println)// 결과 : 1,2,3,4,5
  
  rs = List(1,2,3,4,5,"aa") map( {case i:Int => i + 1} )
  rs.foreach(println)
  //Exception in thread "main" scala.MatchError: aa (of class java.lang.String)
  
}