package akka.basic.creation

import akka.actor.Props
import akka.actor.Actor
import akka.event.Logging
import scala.AnyValCompanion

object TestCreation extends App{
  
  /*
 	ActorRef를 생성하기 위해 ActorSystem에 Props를 인자로 준다.
 	Props은 ActorRef를 생성하기 위한 필요 정보(dispatcher,mailbox등)를 가지며 immutable이다.
 	주의 점들과 Props를 생성하는 방법은 다음과 같이 정리 할 수 있다.
 	
 	1.Props에 Actor를 new 하지 않는다.(companion 에서는 권장)
 	2.Actor에 class parameter가 An actor with AnyVal arguments. 또는 default class parameter가 있는 경우
 	  companion 에서 Props를 return 하는 factory method를 구현하라.
	*/
  
  //1.parameter가 없는 경우
  val props01 = Props[MyActor]
  val props02 = Props(classOf[MyActor])
  val notRecommandProps01 = Props(new MyActor()) // 이렇게 하지 않는다.
  
  //2.class parameter (not case class, not default parameter)
  val props03 = Props(classOf[ActorWithArgs],2)//ok
  //val notRecomandProps02 = Props(new ActorWithArgs(2)) // 이렇게 하지 않는다.
  //val test = Props(classOf[ActorWithArgs02],ParamValue("test"))// not supported scase Test extends AnyVal
  
  //3.class parameter case  class with AnyVal arguments.
  val props04 = ValueActor props01 ActorClassParam(2)
  val props05 = ValueActor props02 ActorClassParam(2)
  //val notOperatedProps = ValueActor props03 ActorClassParam(2)
  
  //4.default parameter
  val props06 = DefaultValueActor props01 3
  val props07 = DefaultValueActor2 props01
  //val notOperatedProps = DefaultValueActor props02 3
  //val notOperatedProps = DefaultValueActor props03 3
  //val notOperatedProps = Props(classOf[DefaultValueActor2])
  
  
}

//################################################################################
//    Normal
//################################################################################
class MyActor extends Actor {
  val log = Logging(context.system, this)

  def receive = {
    case "test" => log.info("received test")
    case _      => log.info("received unknown message")
  }
}

//################################################################################
//    class parameter not case class, not default parameter
//################################################################################
class ActorWithArgs(args:Int) extends Actor {
  val log = Logging(context.system, this)

  def receive = {
    case "test" => log.info("received test")
    case _      => log.info("received unknown message")
  }
}

case class ParamValue(name:String) extends AnyVal

class ActorWithArgs02(paramValue:ParamValue) extends Actor {
  val log = Logging(context.system, this)

  def receive = {
    case "test" => log.info("received test"+ paramValue.name)
    case _      => log.info("received unknown message")
  }
}

//################################################################################
//    class parameter An actor with AnyVal arguments
//################################################################################
case class ActorClassParam(v: Int) extends AnyVal
class ValueActor(value: ActorClassParam) extends Actor {
  def receive = {
    case multiplier: Long => sender() ! (value.v * multiplier)
  }
}

object ValueActor {
  def props01(caseObj:ActorClassParam):Props = Props(new ValueActor(caseObj)) // ok
  def props02(caseObj:ActorClassParam):Props = Props(classOf[ValueActor],caseObj.v)//ok
  def props03(caseObj:ActorClassParam):Props = Props(classOf[ValueActor],caseObj)// not supported
}

//################################################################################
//    default class parameter
//################################################################################
class DefaultValueActor(a: Int, b: Int = 5) extends Actor {
  def receive = {
    case x: Int => sender() ! ((a + x) * b)
  }
}

class DefaultValueActor2(b: Int = 5) extends Actor {
  def receive = {
    case x: Int => sender() ! (x * b)
  }
}

object DefaultValueActor {
  def props01(a:Int):Props = Props(new DefaultValueActor(a)) //ok
  def props02(a:Int):Props = Props(classOf[DefaultValueActor],a.toInt)//not supported
  def props03(a:Int):Props = Props(classOf[DefaultValueActor],a) // not supported
}

object DefaultValueActor2 {
  def props01:Props = Props(new DefaultValueActor2())
}
