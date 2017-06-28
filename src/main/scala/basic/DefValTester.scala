package basic

class DefValTester {
  
  val valueFn = (i:Int) => {
    println("call valueFn")
    i * 3
  }
    
  
  def defFn(i:Int)(f:(Int,Int) => Int):Int => Int = {
    println("call defFn")
    f(i,_:Int)
  }
  
}