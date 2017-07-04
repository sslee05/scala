package basic

import scala.runtime.ScalaNumberProxy
import scala.runtime.RichInt
import scala.Proxy.Typed


object HyperBoundTest {
  
}

case class MyType(value:Int)
//
//case class RichMyType(value:Int) extends MyTypeProxy[RichMyType]{
//  
//  protected def ord = MyTypeOrdering
//}

trait MyTypeProxy[T] extends Ordered[T] with Typed[T] {
    protected def ord:Ordering[T]
    def compare(other:T):Int = ord.compare(self, other)
}

//
//object MyTypeOrdering extends Ordering[RichMyType] {
//    def compare(x: RichMyType, y: RichMyType) = x.value - y.value
//}
//

