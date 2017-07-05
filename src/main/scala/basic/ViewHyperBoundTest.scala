package basic

import scala.runtime.ScalaNumberProxy
import scala.runtime.RichInt
import scala.Proxy.Typed

object ViewHyperBoundTest extends App {
  implicit def convertMyBeanToRichBean(bean:MyBean):RichBean = RichBean(bean)
  
  def maxValue[T <% Ordered[T]](xs:List[T]):T = {
    xs match {
      case Nil => throw new RuntimeException("must not be empty")
      case List(x) => x
      case (x::xs1) => {
        val maxV = maxValue(xs1)
        //T type이 Ordered를 상속한 것이 아닌 Ordered를 지원 할 수 있는 유형이어야 한다.
        //MyBean은 Ordered와 상관이 없지만 implicit RichBean extends Ordered 이므로 가능하다.
        if(x > maxV) x else maxV 
      }
    }
  }
  
  val xs = List(MyBean(1),MyBean(2),MyBean(3),MyBean(5),MyBean(4))
  println(maxValue(xs))

}

case class MyBean(value:Int)
case class RichBean(val self:MyBean) extends OrderedProxy[MyBean] {
  protected def ord = MyOrdering
}


object MyOrdering extends Ordering[MyBean] {
    def compare(x: MyBean, y: MyBean) = x.value - y.value
}


trait OrderedProxy[T] extends Any with Ordered[T] with Typed[T] {
  protected def ord: Ordering[T]

  def compare(y: T) = ord.compare(self, y)
}



