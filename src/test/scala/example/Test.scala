package example

object Test extends App {
  
  def balance(chars: List[Char]): Boolean = {
    
    @annotation.tailrec
    def loop(xs: List[Char], ys: List[Char]): List[Char] = {
      
      xs match {
        case Nil => ys
        case h::t => h match {
          case '(' => loop(xs.tail,h::ys)
          case ')' => if(ys.isEmpty) h::ys else loop(xs.tail,ys.tail)
          case _ => loop(xs.tail,ys) 
        }
      }
      
      /*
      xs match {
        case Nil => ys
        case h :: t =>
          if (h == '(') loop(xs.tail,h :: ys)
          else if (h == ')' && ys.isEmpty) h::ys
          else if (h == ')' && !ys.isEmpty) loop(xs.tail, ys.tail)
          else loop(xs.tail, ys)
      }
      * 
      */
    }

    val result = loop(chars, Nil: List[Char])
    result.isEmpty
  }
  
  val xs:List[Char] = "(()())".toList
  println(balance(xs))
  
  /*
  val r = xs match {
    case h::t => h == '('
    case _ => false
  }
  
  println(r)
  * 
  */
}