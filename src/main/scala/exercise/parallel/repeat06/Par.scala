package exercise.parallel.repeat06

import java.util.concurrent._

object Par {

  type Par[A] = ExecutorService => Future[A]

  case class UnitFuture[A](a: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(): A = a

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  def run[A](es: ExecutorService)(par: Par[A]): Future[A] = par(es)


  def fork[A](par: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call(): A = {
      println(Thread.currentThread().getName)
      run(es)(par).get
    }
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C] = es =>
    UnitFuture(f(run(es)(pa).get, run(es)(pb).get))

  def map[A,B](par: Par[A])(f: A => B): Par[B] =
    map2(par,unit(()))((a,_) => f(a))

  def sortPar(par: Par[List[Int]]): Par[List[Int]] =
    map2(par,unit(()))((a,_) => a.sorted)

  def sortParViaMap(par: Par[List[Int]]): Par[List[Int]] =
    map(par)(a => a.sorted)

  def sequence[A](xs: List[Par[A]]): Par[List[A]] =
    xs.foldRight[Par[List[A]]](unit(List.empty[A]))((a,b) => map2(a,b)((a1,b1) => a1 :: b1))

  def sequenceBalance[A](xs: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = {
    if(xs.isEmpty) unit(Vector.empty[A])
    else if (xs.length == 1) map(xs.head)(a => Vector(a))
    else {
      val (l,r) = xs.splitAt(xs.length / 2)
      map2(sequenceBalance(l), sequenceBalance(r))((a,b) => a ++ b)
    }
  }

  def parMap[A,B](xs: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
    sequenceBalance(xs.map(asyncF(f)))

  def parFilter01[A](xs: List[A])(p: A => Boolean): Par[List[A]] =
    sequence(xs.filter(p).map(a => unit(a)))

  def parFilter02[A](xs: List[A])(p: A => Boolean): Par[List[A]] =
    sequence(xs.flatMap(a => if(p(a)) List(unit(a)) else List.empty[Par[A]]))

  def parFilter[A](xs: List[A])(p: A => Boolean): Par[List[A]] =
    map(sequence(xs.map(asyncF(a => if(p(a)) List(a) else List.empty[A]))))(_.flatten)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => if(run(es)(cond).get) run(es)(t) else run(es)(f)

  def choiceN[A](n: Par[Int])(xs: List[Par[A]]): Par[A] = es =>
    run(es)(xs(run(es)(n).get))

  def choiceMap[K,V](n: Par[K])(xs: Map[K,Par[V]]): Par[V] = es =>
    run(es)(xs(run(es)(n).get))

  def flatMap[A,B](par: Par[A])(f: A => Par[B]): Par[B] = es =>
    run(es)(f(run(es)(par).get))

  def choiceViaFlatMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(if(_) t else f)

  def choiceMapViaFlatMap[K,V](par: Par[K])(xs: Map[K,Par[V]]): Par[V] =
    flatMap(par)(xs(_))

  def join[A](ppar: Par[Par[A]]): Par[A] = es =>
    run(es)(run(es)(ppar).get())

  def joinViaFlatMap[A](ppar: Par[Par[A]]): Par[A] =
    flatMap(ppar)(par => par)

  class ParOpt[+A](par: Par[A]) {
    def map2[B,C](pb: Par[B])(f:(A,B) => C): Par[C] = Par.map2(par,pb)(f)
    def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(par)(f)
    def map[B](f: A => B): Par[B] = Par.map(par)(f)
  }

  implicit def convertParOpt[A](par: Par[A]): ParOpt[A] = new ParOpt(par)

}


object ParApp extends App  {
  import Par._

  val es = Executors.newFixedThreadPool(5)
  val par = unit("a")
  println(par)
  println(run(es)(par).get)

  val par02 = lazyUnit("a" == "a")
  println(par02)
  println(run(es)(par02).get)

  val fn = asyncF((a: Int) => a * 2)
  println(run(es)(fn(2)).get)

  val xs01 = List(3,4,2,1,5,6,7)
  println((run(es)(sortPar(lazyUnit(xs01)))).get())

  es.shutdown()

}

