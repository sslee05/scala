package basic.preview

object Multiplier {
  
  var factor = 2
  val valClosure = (i:Int) =>  {
    println("call_val")
    i * factor
  }
  
  def multiplier02(i:Int):Int = {
    println("call_dev")
    i * factor
  }
  
  
}