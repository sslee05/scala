package akka.basic.creation

import akka.actor.Props
import akka.actor.Actor
import akka.event.Logging

object TestCreation extends App{
  
  val props1 = Props[MyActor]
  val props2 = Props(new ActorWithArgs("arg")) // careful, see below
  val props3 = Props(classOf[ActorWithArgs], "arg") // no support for value class arguments
  
  // NOT RECOMMENDED within another actor:
  // encourages to close over enclosing class
  val props7 = Props(new MyActor)
  
  //case class arguement
  //val valueClassProp = Props(classOf[ValueActor], MyValueClass(5)) // Unsupported RuntimeException
  
  //default class parameter
  //val defaultValueProp1 = Props(classOf[DefaultValueActor], 2.0) // Unsupported
  //val defaultValueProp2 = Props[DefaultValueActor2] // Unsupported
  //val defaultValueProp32 = Props(classOf[DefaultValueActor2])// Unsupported
  val defaultValueProp3 = Props(classOf[DefaultValueActor2],3.0) // Unsupported
  
}


//####### recommand #####################
object DemoActor {
  /**
   * Create Props for an actor of this type.
   *
   * @param magicNumber The magic number to be passed to this actor’s constructor.
   * @return a Props for creating this actor, which can then be further configured
   *         (e.g. calling `.withDispatcher()` on it)
   */
  def props(magicNumber: Int): Props = Props(new DemoActor(magicNumber))
}

class DemoActor(magicNumber: Int) extends Actor {
  def receive = {
    case x: Int => sender() ! (x + magicNumber)
  }
}

class SomeOtherActor extends Actor {
  // Props(new DemoActor(42)) would not be safe
  context.actorOf(DemoActor.props(42), "demo")
  
  def receive = {
    case x: Int => sender() ! (x + magicNumber)
  }
}

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


//################################################################################
case class MyValueClass(v: Int) extends AnyVal
class ValueActor(value: MyValueClass) extends Actor {
  def receive = {
    case multiplier: Long => sender() ! (value.v * multiplier)
  }
}

//################################################################################
class MyActor extends Actor {
  val log = Logging(context.system, this)

  def receive = {
    case "test" => log.info("received test")
    case _      => log.info("received unknown message")
  }
}

class ActorWithArgs(args:String) extends Actor {
  val log = Logging(context.system, this)

  def receive = {
    case "test" => log.info("received test")
    case _      => log.info("received unknown message")
  }
}