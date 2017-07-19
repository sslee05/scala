package basic.preview

object Reduction {
  
  def partial01[A,B,C](a:A,f:(A,B) =>C):B => C = (b:B) => f(a,b)
  
  // A => B => C 이 것에서 => 은 우측으로 묶는다 즉 A => (B => C) 와 같다. 
  def curry[A,B,C](f:(A,B) => C):A => B => C = (a:A) => (b:B) => f(a,b)
  
  def uncurry[A,B,C](f:A => B => C):(A,B) => C = (a:A,b:B) => f(a)(b)
  
  //아래는 f compose g 와 같다. 또는 f andThen g 와 같다.
  def compose[A,B,C](f:B => C,g:A => B):A => C = (a:A) => f(g(a))
  
  def compose02[A,B,C](f:B => C):(A=>B) => A =>C = (g:A => B) => (a:A) => f(g(a))
  def compose03[A,B,C](f:B => C)(g:A => B):A => C = (a:A) => f(g(a))
  def compose04[A,B,C](f:B => C)(g:A => B)(a:A):C = f(g(a))
  def compose05[A,B,C](f:B => C):(A => B) => (A => C) = (g:A => B) => (a:A) => f(g(a))
  def compose06[A,B,C](f:B => C)(g:A => B):A => C = (a:A) =>f(g(a))
  def compose07[A,B,C](f:B => C)(g:A => B)(a:A):C = f(g(a))

}