package basic.parallel

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable

object NonBlocking {

  type Par[A] = ExecutorService => Future[A]

  object Par2 {
    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit): A = {
        println("unitFuture in get function")
        get
      }
      def isCancelled = false
      def cancel(evenIfRunnig: Boolean): Boolean = false
    }

    def fork[A](a: => Par[A]): Par[A] = es => {
      es.submit(new Callable[A] {
        def call = {
          val f = a(es)
          f.get
        }
      })
    }

    def run[A](es: ExecutorService)(par: Par[A]): Future[A] = par(es)

    def unit[A](a: A): Par[A] = es => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def map2[A, B, C](parA: Par[A], parB: Par[B])(f: (A, B) => C): Par[C] = es => {
      val a = parA(es)
      val b = parB(es)
      UnitFuture(f(a.get, b.get))
    }

    def map[A, B](parA: Par[A])(f: A => B): Par[B] =
      map2(parA, unit(()))((a, b) => f(a))

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
      map(parList)(a => a.sorted)

    def sequence[A](xs: List[Par[A]]): Par[List[A]] =
      xs.foldRight[Par[List[A]]](unit(Nil))((a, b) => map2(a, b)((a1, b1) => a1 :: b1))

    def parMap[A, B](xs: List[A])(f: A => B): Par[List[B]] = fork {
      val rs: List[Par[B]] = xs.map(asyncF(f))
      sequence(rs)
    }

    def parFilter[A](xs: List[A])(p: A => Boolean): Par[List[A]] = {
      val rs: List[Par[List[A]]] = xs.map(asyncF[A, List[A]]((x: A) => if ((p(x))) List(x) else Nil))
      map(sequence(rs))(x => x.flatten)
    }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es => {
      if (run(es)(cond).get) t(es)
      else f(es)
    }

    def choiceN[A](n: Par[Int])(xs: List[Par[A]]): Par[A] = es => {
      val rn = run(es)(n).get
      run(es)(xs(rn))
    }

    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => {
      val k = run(es)(key).get
      run(es)(choices(k))
    }

    def flatMap[A, B](par: Par[A])(f: A => Par[B]): Par[B] = es => {
      val av = run(es)(par).get
      run(es)(f(av))
    }

    def choiceViaFlatMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      flatMap(cond)(x => if (x) t else f)

    def choiceMaoViaFlatMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      flatMap(key)(x => choices(x))

    def join[A](a: Par[Par[A]]): Par[A] = es => {
      run(es)(run(es)(a).get())
    }

    def flatMapViaJoin[A, B](par: Par[A])(f: A => Par[B]): Par[B] =
      join(map(par)(f))

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(x => x)

    def map2Via[A, B, C](parA: Par[A], parB: Par[B])(f: (A, B) => C): Par[C] =
      flatMap(parA)(a => map(parB)(b => f(a, b)))

    implicit def convertParOpt[A](par: Par[A]): ParOpt[A] = new ParOpt(par)
    
    class ParOpt[A](p: Par[A]) {
      def map2[B, C](parB: Par[B])(f: (A, B) => C): Par[C] = Par2.map2(p, parB)(f)
    }

  }

}