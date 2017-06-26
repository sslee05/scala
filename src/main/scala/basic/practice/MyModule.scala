package basic.practice

object MyModule {
  
  def abs(n:Int):Int = {
    if(n < 0 ) n * (-1)
    else n
  }
  
  private def format(n:Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(n,abs(n))
  }
  
  def factorial(n:Int):Int = {
    
    @annotation.tailrec
    def loop(n:Int,acum:Int):Int = {
      if(n <= 0) acum
      else loop(n-1,n * acum)
    }
    
    loop(n, 1)
  }
  
  def fibo(n:Int):Int = {
    
    @annotation.tailrec
    def loop(n:Int,cur:Int,pre:Int):Int = {
      if(n <= 1) cur
      else loop(n-1,cur + pre,cur)
    }
    
    loop(n,1,0)
  }
  
  // format function을 Int => Int 를 하므로써 일반화 한다.
  def formatResult(name:String,x:Int)(f:Int => Int):String = {
    val msg = "function is %s is value %d"
    msg.format(name,f(x))
  }
  
  //한가지 type에만 적용되던 단형적 함수(monomorphic function)을 임의의 형식에도 
  //적용되는 다형적 함수(polymorphic function)을 적용 했다.
  def polyTypeHighFunction[A](xs:List[A])(p:A => Boolean):Int = {
    
    @annotation.tailrec
    def loop(xs:List[A],idx:Int)(p:A => Boolean):Int = {
      xs match {
        case Nil => -1
        case (x::xs1) => if(p(x)) idx else loop(xs1,idx + 1)(p)
      }
    }
    
    loop(xs,0)(p)
  }
  
  def main(args:Array[String]):Unit = {
    println(format(-42))
    println(factorial(3))
    println(fibo(7))
    println(formatResult("factorial",3)(factorial))
    println(formatResult("fivo",5)(fibo))
    
    val xs = List(1,2,3,4,5,6)
    println(polyTypeHighFunction(xs)(x => x > 3))
  }
  
}