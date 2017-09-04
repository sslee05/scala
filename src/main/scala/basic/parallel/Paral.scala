package basic.parallel

//import java.util.concurrent.ExecutorService
//import scala.concurrent.Future
import java.util.concurrent._
import language.implicitConversions

object Paral {
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = {
      println("unitFuture in get function")
      get
    }
    def isCancelled = false
    def cancel(evenIfRunnig: Boolean): Boolean = false
    
  }
  
  type Paral[A] = ExecutorService => Future[A]
  
  def unit[A](a: A): Paral[A] = { 
    println(Thread.currentThread.getName +":"+Thread.currentThread.getId +": call unit fn:"+ a)
    Thread.sleep(2000)
    es => {
    println(Thread.currentThread.getName +":"+Thread.currentThread.getId +": call unitProcess fn:"+ a)
    Thread.sleep(2000)
    UnitFuture(a)} 
  }
  
  def map2[A,B,C](a: Paral[A], b: Paral[B])(f: (A,B) => C): Paral[C] = es => {
    println(Thread.currentThread.getName +":"+Thread.currentThread.getId +":"+"startMap2")
    
    val futureA = a(es)
    val futureB = b(es)
    println(Thread.currentThread.getName +":"+Thread.currentThread.getId +":"+ futureA +":" + futureB)
    UnitFuture(f(futureA.get,futureB.get))
  }
  
  def fork[A](a: => Paral[A]): Paral[A] = es => 
    es.submit(new Callable[A]{
      def call = {
        val tt = a(es)
        val r = tt.get
        println(Thread.currentThread.getName +":"+Thread.currentThread.getId +":"+ r)
        r
      }
    })
    
  def lazyUnit[A](a: => A): Paral[A] = fork(unit(a))
  def run[A](a: Paral[A]): Paral[A] = es => a(es)
  
  def asyncF[A,B](f: A => B): A => Paral[B] = a => lazyUnit(f(a))
  
  def sortParal(xs: Paral[List[Int]]): Paral[List[Int]] = 
    map2(xs, unit(()))((a,b) => a.sorted)
    
  def sequence[A](ps: List[Paral[A]]): Paral[List[A]] = 
    ps.foldRight[Paral[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))
    
  def paralMap[A,B](xs: List[A])(f: A => B): Paral[List[B]] = fork {
    val fbs: List[Paral[B]] = xs.map(asyncF(f))
    sequence(fbs)
  }
}

object ParalDrive extends App {
  import basic.parallel.Paral._
  val es = Executors.newFixedThreadPool(5)
  //println(Thread.currentThread.getName +":"+Thread.currentThread.getId +":"+ unit(1)(es).get)
  //println(Thread.currentThread.getName +":"+Thread.currentThread.getId +":"+ fork(unit(2))(es).get)
  //Thread.sleep(10000)
  
  //println(map2(unit(1),unit(2))((a,b) => a + b)(es).get)
  //println(fork(map2(unit(1),unit(2))((a,b) => a + b))(es).get)
  //println(map2(fork(unit(1)),fork(unit(2)))((a,b) => a + b)(es).get)
  
  //test asyncF
  //println(map2(asyncF((a:Int) => a+ 1)(2),asyncF((a:Int) => a + 2)(3))((a,b) => a + b)(es).get)
  
  //test sortParal
  val xs = List(5,4,3,6,1)
  //println(sortParal(fork(unit(xs)))(es))
  
  //test paralMap
  println(paralMap(xs)( a => a * a)(es).get)
  
  //val fbs: List[Paral[Int]] = xs.map(asyncF(a => a * a))
  //println(fbs.foldRight(0)( (a,b) => a(es).get + b))
  println("end")
  
  Thread.sleep(3000)
}