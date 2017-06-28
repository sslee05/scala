package akka.basic

import akka.actor.Actor
import akka.event.Logging
import akka.actor.ActorRef

class PongActor extends Actor {
  
  val logger = Logging(context.system,this)
  
  def receive = {
    case msg:String => {
      logger.info(s"this is PongActor receive message is $msg")
      sender() ! "Ping"      
    }
  }
  
}