package basic

object HFunction01 extends App{
  
  val r1 = factorial(3)
  println(r1)
  
  val r2 = fibo(7)
  println(r2)
  
  def factorial(n:Int):Int = {
    
    @annotation.tailrec
    def loop(n:Int,acum:Int):Int = {
      if(n <= 0) acum
      else loop(n-1,n * acum)
    }
    
    loop(n,1)
  }
  
  def fibo(n:Int):Int = {
    
    @annotation.tailrec
    def loop(n:Int,cur:Int,pre:Int):Int = {
      if(n <= 0) pre
      else loop(n-1,cur+pre,cur)
    }
    
    loop(n,1,0)
  }
}