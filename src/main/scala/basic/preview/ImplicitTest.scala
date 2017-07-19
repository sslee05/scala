package basic.preview

object ImplicitTest extends App {
  
  //type 의 암시적 변환
  //이것은 어디까지나 예이다 손실이 있는 변환은 좋지 못하다.
  //Double에서 Int로의 변환은 손실이 일어 난다.
  /*
  implicit def convert2Double(value:Double) = value.toInt
  val i:Int = 3.5
  println(i)
  */
  
  
  val bean:Rational = Rational(2,4)
  val other:Rational = Rational(4,6)
  
  val result = bean + other
  println(result)
  
  //1 + other// compile error
  implicit def convertIntToRational(value:Int) = Rational(value,0)
  val result02 = 1 + other
  println(result02)
  
  val channel = new Channel()
  channel sendMsg "hellow Java"
  
}

class Channel {
  def sendMsg(msg:String)(implicit sender:MySender):Unit = {
    println(msg +" sender:"+sender.name)
  }
}

object MySender {
  implicit val sender:MySender = MySender("MyCaller") 
}

case class MySender(name:String) 

case class Rational(x:Int,y:Int) {
  def + (other:Rational):Rational = {
    Rational(this.x + other.x , this.y + other.y)
  }
  
  override def toString:String = "x:"+this.x + " y:"+this.y
}