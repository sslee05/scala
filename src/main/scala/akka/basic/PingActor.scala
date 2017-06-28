package akka.basic

import akka.actor.Actor
import akka.actor.ActorRef
import akka.event.Logging
import akka.actor.Props
import akka.actor.ActorSystem

class PingActor extends Actor{
  
  val logger = Logging(context.system,this)
  var child:ActorRef = _
  
  def receive = {
    case msg @ "Ping" => {
      logger.info(s"Received a $msg !")
      child ! "Pong"
    }
    case other @ _ => logger.info(s"unExcepted message $other")
  }
  
  override def preStart = {
    logger.info("preStart")
    child = context.actorOf(Props(new PongActor()))
  }
  
  override def preRestart(reason:Throwable,msg:Option[Any]) = {
    logger.info(s"preRestart error message is $msg")
    super.preRestart(reason, msg)
  }
  
  override def postRestart(reason:Throwable) = {
    logger.info(s"postReStart error message is $reason.msg")
    super.postRestart(reason)
  }
  
  override def postStop = {
    logger.debug("PingActor is shutdown")
  }
  
}

object PingActorApp extends App {
  val actorSystem = ActorSystem("pingpong-System")
  val pingActor = actorSystem.actorOf(Props[PingActor], "pingActor")
  pingActor ! "Ping"
  
  Thread.sleep(500);
  
  actorSystem.terminate()
}

