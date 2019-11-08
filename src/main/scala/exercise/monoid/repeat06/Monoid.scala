package exercise.monoid.repeat06

import basic.parallel.Par

trait Monoid[A] {
  def op(a: A, b: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a: String, b: String): String = a  + b
    def zero: String = ""
  }

  val intPlusMonoid = new Monoid[Int] {
    def op(a: Int, b: Int): Int = a + b
    def zero: Int = 0
  }

  val intProductMonoid = new Monoid[Int] {
    def op(a: Int, b: Int): Int = a * b
    def zero: Int = 1
  }

  val booleanOrMonoid = new Monoid[Boolean] {
    def op(a: Boolean , b: Boolean): Boolean = a || b
    def zero: Boolean = false
  }

  val booleanAndMonoid = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean): Boolean = a && b
    def zero: Boolean = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a: Option[A], b: Option[A]): Option[A] = a orElse b
    def zero:Option[A] = Option.empty[A]
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A): A => A = f compose g
    def zero: A => A = a => a
  }

  def listFold[A](xs: List[A])(m: Monoid[A]): A =
    xs.foldRight(m.zero)((a,b) => m.op(a,b))

  def listFoldMap[A,B](xs: List[A],f: A => B)(m: Monoid[B]): B =
    xs.foldRight(m.zero)((a,b) => m.op(f(a),b))

  def foldRightList[A,B](xs: List[A],z: B)(f: (A,B) => B): B =
    listFoldMap[A, B => B](xs,f.curried)(endoMonoid[B])(z)

  def dualMonoid[A](m: Monoid[A => A]): Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A): A => A = m.op(g,f)
    def zero: A => A  = m.zero
  }

  def foldLeftList[A,B](xs: List[A],z: B)(f: (B,A) => B): B =
    listFoldMap[A,B => B](xs, a => (b:B) => f(b,a))(dualMonoid(endoMonoid[B]))(z)

  def foldMapV[A,B](xs: IndexedSeq[A], f: A => B)(m: Monoid[B]): B = {
    if(xs.isEmpty) m.zero
    else if(xs.length == 1) f(xs.head)
    else {
      val (l,r) = xs.splitAt(xs.length / 2)
      m.op(foldMapV(l,f)(m), foldMapV(r,f)(m))
    }
  }

  import basic.parallel.Par._

  def parMonoid[A](m: Monoid[A]) = new Monoid[Par[A]] {
    def op(pa: Par[A], pb: Par[A]): Par[A] = pa.map2(pb)(m.op)
    def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMapV[A,B](xs: IndexedSeq[A], f: A => B)(m: Monoid[B]): Par[B] =
    foldMapV(xs, asyncF(f))(parMonoid(m))

  def mergeOption[A](m: Monoid[A]): Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(ma: Option[A], mb: Option[A]): Option[A] = ma.flatMap(a => mb.map(b => m.op(a,b)))
    def zero: Option[A] = Some(m.zero)
  }

  def mergeMap[K,V](m: Monoid[V]): Monoid[Map[K,V]] = new Monoid[Map[K,V]] {
    def op(ma: Map[K,V], mb: Map[K,V]): Map[K,V] = (ma.keySet ++ mb.keySet)
      .foldRight(Map.empty[K,V])((k, zb) => zb.updated(k, m.op(ma.getOrElse(k, m.zero), mb.getOrElse(k,m.zero))))

    def zero: Map[K,V] = Map.empty
  }

  //Option[Map[String,Map[String,Int]]]
  val compositionMonoid: Monoid[Option[Map[String, Map[String, Int]]]] =
    mergeOption(mergeMap(mergeMap(intPlusMonoid)))

  def tupleMonoid[A,B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)] {
    def op(a: (A,B), b: (A,B)): (A,B) = (ma.op(a._1, b._1), mb.op(a._2, b._2))
    def zero: (A,B) = (ma.zero, mb.zero)
  }

  def functionMonoid[A,B](m: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(f: A => B, g: A => B): A => B = a => m.op(f(a),g(a))
    def zero: A => B = a => m.zero
  }

  def bag[A](xs: IndexedSeq[A]): Map[A,Int] =
    foldMapV(xs, (a:A) => Map(a -> 1))(mergeMap(intPlusMonoid))
}



object MonoidApp extends App {
  import exercise.monoid.repeat06.Monoid._

  println(bag(Vector("a","rose","is","a","rose")))
}
