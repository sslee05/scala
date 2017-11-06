package exercise.parallel.repeat03

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable


/**
 * cpu 안에 core(연산처리) + controller + cache memory
 * cpu는 공장 core는 일하는 사람 클럭수는 일하는 사람의 일하는 속도
 * Par의 생성에 따른 동기,비동기 처리방식이 결정됨.
 */
//ex-01) Par object를 만들고 ExecutorService 를 받아 Future를 반환하는 type을 만들어라.
object Par { self =>
  
  type Par[A] = ExecutorService => Future[A]
  
  //ex-02)Future를 상속한 ExecutorService를 사용하지 않고 값을 받아서 바로 결과를 돌려주는 UnitFuture class를 만들어라.
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = {
      get
    }
    
    def isCancelled = false
    def cancel(eventIfRunning: Boolean): Boolean  = false
  }

  //ex-02) 값을 받아 바로 평가 해서 결과를 UnitFuture로 돌려 주는 unit함수를 구현하라.
  def unit[A](a: A): Par[A] = ex => UnitFuture(a)
  
  //ex-03) ExecutorService와 Par를 받아서 실행해서 결과를 Future로 받는 run 함수를 작성하라.
  def run[A](es: ExecutorService)(par: Par[A]): Future[A] = par(es)
  
  //ex-04) Par를 평가되지 않는 인수를 받아 동시적으로 평가됨을 표시하는 것으로 실제평가는 run에 강제되어야 실행되는 fork 함수를 작성하라.
  def fork[A](par: => Par[A]): Par[A] = es => {
    es.submit(new Callable[A] {
      def call  = {
        val f = par(es)
        f.get
      }
    })
  }
  
  //ex-05) 평가되지 않는 인수를 Par로 감싸고, 그것을 병렬 평가 대상으로 표시하는 함수를 작성하라.
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  
  //ex-06) 임이의 함수 A => B를 그결과가 비동기적으로 평가되는 함수로 반환하는 함수를 lazyUnit을 이용하여 작성하라.
  def asyncF[A,B](f: A => B): A => Par[B] = a => fork(unit(f(a)))
  
  //ex-07) 다음의 map2함수를 구현하라.
  //Par가 lazyUnit으로 생성시 비동기적으로 실행 unit으로 생성시 동기적으로 실행, Par가 unit으로 생성시 동기적으로 실행
  def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C] = es => {
    val fa = pa(es)
    val fb = pb(es)
    UnitFuture(f(fa.get,fb.get))
  }
  
  //ex-08) run을 실행하지 않고 정렬하는 다음의 함수를 구현하라. 이는 Par[C](es) 시에 실제로 정렬된다.
  def sortPar(xp: Par[List[Int]]): Par[List[Int]] = 
    map2(xp,unit(()))((a,_) => a.sorted)
    
  //ex-09)map을 map2로 구현하라.이는 Par[B](es) 시에 실제로 정렬된다.
  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa,unit(()))((a,_) => f(a))
    
  //ex-10) sortPart를 map를 이용하여 구현하라.
  def sortParViaMap(xp: Par[List[Int]]): Par[List[Int]] =
    map(xp)(a => a.sorted)
    
  //ex-11) sequence를 구현하라.
  def sequence[A](xs: List[Par[A]]): Par[List[A]] =
    xs.foldRight[Par[List[A]]](unit(Nil))((a,b) => map2(a,b)(_::_))
 
  //ex-12) 목록의 반을 분기해서 처리하는 sequnceBalance를 구현하라.
  def sequenceBalance[A](xs: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = {
    if(xs.isEmpty) unit(Vector.empty)
    else if(xs.length == 1) map(xs.head)(a => Vector(a))
    else {
      val (l,r) = xs.splitAt(xs.length / 2)
      map2(sequenceBalance(l), sequenceBalance(r))((a,b) => a ++ b)
    }
  }
  
  //ex-13)N개의 병렬계산을 결합하는 함수를 만들어라.
  // Par[IndexedSeq[B]](es) 할때 f함수의 처리를 병렬로 처리하게 된다.
  def parMap[A,B](xs: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
    sequenceBalance(xs.map(asyncF(f)))
    
  //ex-14) 목록의 요소들을 병렬로 filter하는 parFilter를 구현하라.
  def parFilter[A](xs: List[A])(f: A => Boolean): Par[List[A]] = 
    map(sequence(xs.map(asyncF( (a:A)  => if(f(a)) List(a) else Nil ))))(_.flatten)
  
  //ex-15)cond이 true이면 t를 false이면 f를 실행하는 함수를 작성하라.
  //이는 cond의 실행결과까지 block된다.
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es => 
    if(run(es)(cond).get) run(es)(t) else run(es)(f)
  
  //ex-16)n의 순번에 따른 xs안의 Par를 실행하는 함수를 작성하라.
  def choiceN[A](n: Par[Int])(xs: List[Par[A]]): Par[A] = es => 
    run(es)(xs(run(es)(n).get))
    
  //ex-17) key에 해당하는 choices map의 Par를 실행하는 것을 구현하라.
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = es =>
    run(es)(choices(run(es)(key).get))
    
  //ex-18)이를 일반화한 즉 선행의 계산에 따른 후행의 계산을 작성하라.
  def flatMap[A,B](mp: Par[A])(f: A => Par[B]): Par[B] = es => 
    run(es)(f(run(es)(mp).get))
    
  //ex-19)choice를 flatMap으로 재구현 하라.
  def choiceViaFlatMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = 
    flatMap(cond)(a => if(a) t else f)
    
  //ex-20)choniceMap를 flatMap으로 재구현 하라.
  def choiceMapViaFlatMap[K,V](key: Par[K])(xs: Map[K,Par[V]]): Par[V] = 
    flatMap(key)(k => xs(k))
    
  //ex-21)join 함수를 구현하라.(flatMap를 사용하지 말고)
  def join[A](ppa: Par[Par[A]]): Par[A] = es =>
    run(es)(run(es)(ppa).get())
  
  //ex-22)flatMap를 이용하여 join를 재구현하라.
  def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] = 
    flatMap(ppa)(pa => pa)
  
  implicit def convertParOpt[A](par: Par[A]): ParOpt[A] = new ParOpt(par)
    
  class ParOpt[A](p: Par[A]) {
    def map2[B,C](pb: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p,pb)(f)
    def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
  }
    
}

import exercise.parallel.repeat03.Par._
import java.util.concurrent.Executors

object ParDriver extends App {
  
  //println(Thread.currentThread.getName +":"+Thread.currentThread.getId +": main thread")
  //pa 와 pb가 비동기로 실행시 
  //val pa = lazyUnit(1)
  //val pb = lazyUnit(2)
  
  val pa = unit(1)
  val pb = unit(2)
  
  val pc = map2(pa,pb)((a,b) => a + b)
  
  val es = Executors.newFixedThreadPool(5)
  val fc = pc(es)
  println(fc.get)
  
  //###################
  val xs01 = List(3,1,2)
  val p01 = unit(xs01)
  val rs01 = sortPar(p01)
  println(rs01)
  println(rs01(es))
}