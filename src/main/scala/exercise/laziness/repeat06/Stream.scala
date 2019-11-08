package exercise.laziness.repeat06

import exercise.laziness.repeat06.Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def toListNotTailRec: List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h() :: t().toList
  }

  def tailRecToList: List[A] = {
    def go(sx: Stream[A], xs: List[A]): List[A] = sx match {
      case Empty => xs.reverse
      case Cons(h,t) => go(t(), h()::xs)
    }

    go(this, List.empty[A])
  }

  def toListNotRevers: List[A] = {
    type FN = List[A] => List[A]

    @tailrec
    def go[B](sx: Stream[A], z: B)(f: (B,A) => B) : B = sx match {
      case Empty => z
      case Cons(h, t) => go(t(), f(z, h()))(f)
    }

    go(this, (y: List[A]) => y)((b,a) => (x: List[A]) => b(a::x))(List.empty[A])
  }

  def toList: List[A] = {
    var rb = new scala.collection.mutable.ListBuffer[A]

    def go(sx: Stream[A]): List[A] = sx match {
      case Empty => rb.toList
      case Cons(h,t) => {
        rb += h()
        go(t())
      }
    }

    go(this)

  }

  def takenOrigin(n: Int): List[A] = this match {
    case Cons(h,t) if n > 0 => h() :: t().takenOrigin(n-1)
    case _ => List.empty[A]
  }

  def dropOrigin(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().dropOrigin(n -1)
    case _ => this
  }

  def takeWhileOrigin(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if(p(h())) cons[A](h(), t() takeWhileOrigin p) else t() takeWhileOrigin p
    case _ => empty[A]
  }

  def exist(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) || (t() exist p)
    case _ => false
  }

  def foldMap[B](z: => B )(f: ( => B, A) => B): Stream[B] = this match {
    case Cons(h,t) => {
      println("a")
      t().foldMap(f(z,h()))(f)
    }
    case _ => empty[B]
  }

  def foldRight[B](z: => B)(f:( => B,A) => B): B = this match {
    case Cons(h,t) => f(t().foldRight(z)(f),h())
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((b,a) => p(a) && b)

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((b,a) => if(p(a)) cons(a, b) else b)

  def headOption:Option[A] = foldRight(Option.empty[A])((b,a) => Some(a) orElse b)

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((b,a) => cons(f(a),b))

  def append[B >: A](xs:  => Stream[B]): Stream[B] = this.foldRight(xs)((b,a) => cons(a,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(empty[B])((b,a) => f(a).append(b))

  def filter(p: A => Boolean):Stream[A] = this.foldRight(empty[A])((b,a) => if(p(a)) cons(a,b) else b)

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h,t) => Some((f(h()), t()))
    case Empty => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this,n)){
    case (Cons(h,t), n1) if n > 0 => Some((h(),(t(),n1-1)))
    case _ => Option.empty
  }

  def takeWhileViaUnFold(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h,t)  if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B,C](sx: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this,sx)){
    case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(),h2()),(t1(),t2())))
    case _ => None
  }

  def zipAll[B](sx: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this,sx)){
    case (Cons(h1,t1),Cons(h2,t2)) => Some(( (Some(h1()), Some(h2())), (t1(), t2()) ))
    case (Cons(h1,t1),_) => Some(( (Some(h1()) , None), (t1(),empty[B] )))
    case (_, Cons(h2,t2)) => Some(( (None, Some(h2())) , (empty[A], t2())))
    case _ => None
  }

  def hasElement[B >: A](sx: Stream[B]): Boolean =
    foldRight(false)((b,a) => sx.exist(a1 => a1 == a) || b)

  def startsWith[B >: A](sx: Stream[B]): Boolean =
    this.zipAll(sx).takeWhileViaUnFold(a => !(a._1.isEmpty || a._2.isEmpty)).forAll(t => t._1.flatMap(a => t._2.map(b => a == b)).get)

  def tails:Stream[Stream[A]] = unfold(this){
    case Cons(h,t) => Some((cons(h(),t()), t()))
    case Empty => None
  }

  def subSequence[B >: A](sx: Stream[B]): Boolean =
    this.tails.exist(s => s.startsWith(sx))



}

sealed case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]
case object Empty extends Stream[Nothing]

object Stream {

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t

    Cons(() => head,() => tail)
  }

  def empty[A]: Stream[A] = Empty;

  def apply[A](xs: A *): Stream[A] = {
    if(xs.isEmpty) empty
    else {
      cons(xs.head, apply(xs.tail: _ *))
    }
  }

  def constant[A](a: A): Stream[A] = cons(a,constant(a))

  def from(n: Int):Stream[Int] = cons(n, from(n+1))

  def fibo(pre: Int, cur: Int): Stream[Int] = cons(pre, fibo(cur, pre + cur))

  def unfold[S,A](s: S)(f: S => Option[(A,S)]): Stream[A] = f(s) match {
    case Some((a,s1)) => cons(a, unfold(s1)(f))
    case None => empty[A]
  }

  def fiboViaUnFold(pre: Int, cur: Int): Stream[Int] = unfold((pre,cur)){
    case (p, c) => Some((p,(c,p + c)))
    case _ => None
  }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n){
    case n => Some((n, n+1))
  }

  def ones: Stream[Int] = unfold(1){ c => Some((c,c))}
}

object StreamApp extends App {
  println("hello world")

  println("ex01")
  val sx01 = cons("a",cons[String]("b", cons("c",empty)))
  sx01.toList.foreach(println)

  println("ex02")
  sx01.tailRecToList.foreach(println)

  println("ex03")
  sx01.toListNotRevers.foreach(println)

  println("ex04")
  sx01.toList.foreach(println)

  println("ex05")
  sx01.takenOrigin(2).foreach(println)

  println("ex06")
  sx01.dropOrigin(2).toList.foreach(println)

  println("ex07")
  sx01.takeWhileOrigin(x => x == "c").toList.foreach(println)

  println("ex08")
  sx01.foldMap("")((b,a) => b+a)

  println("ex09")
  val sx02 = cons(1,cons[Int](2, cons(3,empty)))
  println(sx02.foldRight( 1 / 0)((b,a) => if(a > 0) a else a  * b ))

  println("ex10")
  println(sx02.forAll(a => a > 5))

  println("ex11")
  println(sx02.takeWhile(a => a < 3).toList)

  println("ex12")
  println(sx02.headOption)

  println("ex13")
  println(sx02.map(a => "type=>"+a).toList)

  println("ex14")
  println(sx02.map(a => String.valueOf(a)).append(sx01).toList)

  println("ex15")
  println(sx02.flatMap(a => cons(a,empty[Int])).toList)

  println("ex16")
  println(sx02.filter(a => a > 2).toList)

  println("ex17")
  println(Stream.constant("a").takenOrigin(3).toList)

  println("ex18")
  println(Stream.from(1).takenOrigin(5).toList)

  println("ex19")
  println(Stream.fibo(0,1).takenOrigin(10).toList)

  println("ex20")
  println(fiboViaUnFold(0,1).takenOrigin(10).toList)

  println("ex21")
  println(fromViaUnfold(1).takenOrigin(5).toList)

  println("ex22")
  println(ones.takenOrigin(5).toList)

  println("ex23")
  println(sx02.mapViaUnfold(a => String.valueOf(a * a)).toList)

  println("ex24")
  println(sx02.takeViaUnfold(2).toList)

  println("ex25")
  println(sx02.takeWhileViaUnFold(a => a > 0).toList)

  println("ex26")
  println(sx02.zipWith(sx01)((i,s) => s + String.valueOf(i)).toList)

  println("ex27")
  println(sx01.zipAll(sx02).toList)

  val sx03 = cons("a",cons[String]("b", cons("c",cons("d",empty[String]))))
  println("ex28")
  println(sx01.hasElement(sx03))
  println (sx03.hasElement(sx01))

  val sx04 = cons("d",cons[String]("b", cons("c",empty)))
  println("ex29")
  println(sx01.startsWith(sx03))
  println(sx01.startsWith(sx04))

  println("ex30")
  println(sx01.tails.toList.foreach(s => println(s.toList)))

  val sx05 = cons("b",cons("c",empty[String]))
  println(sx01.subSequence(sx05))

}