package akka.basic.creation

import akka.actor.Props
import akka.actor.Actor
import akka.event.Logging

object TestCreation extends App{
  
  //ActorSystem 을 통한 ActorRef 를 생성하기 위한 Props의 생성 종류와 주의 점들 
  //Actor 생성은 대상 Actor에 class parameter가 없을 경우는 간단하다.
  //Actor 에 class parameter가 있을 경우 new 를 하지 않으며, class parameter가 
  //case class 인경우,default class parameter인 경우 companion 객체로 factory method를 제공하게 한다.
  
  //1.parameter가 없는 경우
  val props01 = Props[MyActor]
  val props02 = Props(classOf[MyActor])
  val notRecommandProps01 = Props(new MyActor()) // 이렇게 하지 않는다.
  
  //2.class parameter (not case class, not default parameter)
  val props03 = Props(classOf[ActorWithArgs],"args")
  val notRecomandProps02 = Props(new ActorWithArgs("arg")) // 이렇게 하지 않는다.
  
  //3.class parameter case  class 
  val props04 = ValueActor props01 ActorClassParam(2)
  //val notOperatedProps = ValueActor props02 ActorClassParam(2)
  //val notOperatedProps = ValueActor props03 ActorClassParam(2)
  
  //4.default parameter
  val props05 = DefaultValueActor props01 3
  //val notOperatedProps = DefaultValueActor props02 3
  //val notOperatedProps = DefaultValueActor props03 3
  
  
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
class ActorWithArgs(args:String) extends Actor {
  val log = Logging(context.system, this)

  def receive = {
    case "test" => log.info("received test")
    case _      => log.info("received unknown message")
  }
}

//################################################################################
//    class parameter case class
//################################################################################
case class ActorClassParam(v: Int) extends AnyVal
class ValueActor(value: ActorClassParam) extends Actor {
  def receive = {
    case multiplier: Long => sender() ! (value.v * multiplier)
  }
}

object ValueActor {
  def props01(caseObj:ActorClassParam):Props = Props(new ValueActor(caseObj)) // ok
  def props02(caseObj:ActorClassParam):Props = Props(classOf[ActorClassParam],caseObj.v)//ok
  def props03(caseObj:ActorClassParam):Props = Props(classOf[ActorClassParam],caseObj)// not 
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
