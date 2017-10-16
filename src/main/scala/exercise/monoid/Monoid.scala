package exercise.monoid

//ex-01) monoid를 모델링 하라.
trait Monoid[A] {
  def zero: A
  def op(a: A, b: A): A
}

object Monoid {

  //ex-02) String Monoid를 작성하라.
  val stringMonoid: Monoid[String] = ???

  //ex-03) Int에 대한 + 연산 monoid를 작성하라.
  val intAddMonoid: Monoid[Int] = ???

  //ex-04) Int에 대한 곱의 연산에 대한 Monoid를 작성하라.
  val intProductMonoid: Monoid[Int] = ???

  //ex-05) Boolean의  or 연산에 대한 Monoid를 작성하라. 
  val booleanOrMonoid: Monoid[Boolean] = ???
  
  //ex-06) Boolean의 and 연산에 대한 Monoid를 작성하라.
  val booleanAndMonoid: Monoid[Boolean] = ???

  //ex-07) Option에 대한 Monoid를 작성하라.
  def optionMonoid[A]: Monoid[Option[A]] = ???

  //ex-08) 자기함수에 대한 Monoid를 작성하라.
  def endoMonoid[A]: Monoid[A => A] = ???
  
  //ex-09)list에 대한 접기 함수를 monoid를 이용하여 구현
  def listFold[A](xs: List[A])(m: Monoid[A]): A = ???
    
  //ex-10) List에 대한 시작과 결과의 형식이 다른 목록의 접기 함수를 monoid를 사용하여 작성하라.
  def listFoldMap[A,B](xs: List[A])(f: A => B)(m: Monoid[B]): B = ???
    
    
  //ex-11) List에 대한 foldRight를 listFoldMap 함수를 이용하여 구현하라
  // foldRight를 foldMap를 이용해서 구현하라.
  //f:(A,B) => B 는 f.curried : A => (B => B) 여기서 B => B type은 호출하고 있는 foldMap의 또 다른 B type 이다.
  // 즉 type B[B] = B => B 
  // foldMap의 B type을 B => B 로 생각하면 됨.
  //f1(f2(f3(z))) <= f comose g
  def foldRightViaFoldMap[A,B](xs: List[A])(z: B)(f: (A,B) => B): B = ???
  
  //ex-12) foldLeft를 listFoldMap을 이용해서 구현하라
  def dualMonoid[A](m: Monoid[A]) = ???
  
  //f(3(f2(f1(z)))) <= g compose f
  def foldLeftViaFoldMap[A,B](xs: List[A])(z: B)(f: (B,A) => B): B = ???
    
  
  //ex-13) IndexedSeq에 대한 listFoldMap를 구현하라.
  //구현은 반드시 순차열을 둘로 분할해서 재귀적으로 각 절반을 처리하고 그 결과들을 monoid를 이용해서 결합해야 한다.
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = ???
  
  //ex-14) foldMapV의 병렬버전도 구현해라.
  //parallel chapter에서 foldMap를 만들었다 이를 monoid를 이용해서 구현하라. 이때  
  //승격 함수인 Monoid[A] => Monoid[Par[A]] 가 필요하다.
  import basic.parallel.Par._
  def par[A](m: Monoid[A]): Monoid[Par[A]] = ???
  
  def parFoldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = ???
  
  //ex-15)foldMap를 이용해서 주어진 IndexedSeq[Int] 가 정렬되어 있는지 점검하라.
  //Monoid를 이용할 것
  def ordered(xs: IndexedSeq[Int]): Boolean = ???
  
  //ex-21)2개의 Option(Option(Int)) 의 자료구조의 병합을 위의 monoide들을 합성하여 구현하라.
  def mergeOption[O](m: Monoid[O]): Monoid[Option[O]] = ???
  
  //ex-22)2개의 Map[String,Map[String,Int]] 의 자료구조의 병합을 위의 monoide들을 합성하여 구현하라
  def mergeMapMonoid[K,V](m: Monoid[V]): Monoid[Map[K,V]] = ???
  
  //ex-23) 2개의 Option[Map[String,Map[String,Int]]] 의 자료구조의 병합을 위의 2문제를 이용하여 작성하라.
  val mMonoid2: Monoid[Option[Map[String,Map[String,Int]]]] = ???
 
  //ex-24)monoid 곱을 구현하라.
  def productMonoid[A,B](m1: Monoid[A], m2: Monoid[B]): Monoid[(A,B)] = ???
  
  //ex-25)결과가 monoid 인 함수들에 대한 monoid instance를 작성하라.
  def functionMonoid[A,B](m: Monoid[B]): Monoid[A => B] = ???
  
  //ex-26) bag라는 자료구조는 집합처럼 각 요소를 하나씩만 담되, 그 요소의 출현 횟수도 기억한다.
  //구체적으로, 자루는 각 요소가 key 이고 그 요소의 출현 횟수가 값인 Map[요소,출현횟수] 으로 표현된다.
  // bag(Vector("a","rose","is","a","rose")) 일때 결과는 
  // res0: Map[String,Int] = Map(a -> 2, is -> 1, rose -> 2)  가 나오도록 
  // monoid들을 이용해서 IndexedSeq 로 부터 bag 를 구현하라.
  def bag[A](xs: IndexedSeq[A]): Map[A,Int] = ???
    
}

//ex-16)여러 자료구조에서 fold의 공통점을 뽑아 Foldable trait를 구현하라.
import exercise.monoid.Monoid._
import java.util.concurrent.Executors

trait Foldable[F[_]] {
  
  def foldRight[A,B](xs: F[A])(z: B)(f: (A,B) => B): B = ???
  
  def foldLeft[A,B](xs: F[A])(z: B)(f: (B,A) => B): B = ???
  
  def foldMap[A,B](xs: F[A])(f: A => B)(m: Monoid[B]): B = ???
  
  def concat[A](xs: F[A])(m: Monoid[A]): A = ???
  
  def toList[A](xs: F[A]): List[A] = ???
}


//ex-17)
class FoldableList extends Foldable[List] {
  override def foldRight[A,B](xs: List[A])(z: B)(f: (A,B) => B): B = ???
    
  override def foldLeft[A,B](xs: List[A])(z: B)(f: (B,A) =>B): B = ???
    
  override def foldMap[A,B](xs: List[A])(f: A => B)(m: Monoid[B]): B = ???
    
  override def toList[A](xs: List[A]): List[A] = ???
}

//ex-18) 다음을 구현하라.
class FoldableIndexedSeq extends Foldable[IndexedSeq]  {
  override def foldRight[A,B](xs: IndexedSeq[A])(z: B)(f: (A,B) => B): B = ???
    
  override def foldLeft[A,B](xs: IndexedSeq[A])(z: B)(f: (B,A) => B): B = ???
    
  override def foldMap[A,B](xs: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B = ???
}

//ex-19) 다음을 구현하라.
class FoldableStream extends Foldable[Stream] {
  override def foldRight[A,B](xs: Stream[A])(z: B)(f: (A,B) => B): B = ???
    
  override def foldLeft[A,B](xs: Stream[A])(z: B)(f: (B,A) => B): B = ???
}

//ex-20) 다음을 구현하라.
sealed trait Tree[+A]
case class Leaf[A](v: A) extends Tree[A]
case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]

class FoldableTree extends Foldable[Tree] {
  override def foldRight[A,B](t: Tree[A])(z: B)(f: (A,B) => B) : B = ???
    
  
  override def foldLeft[A,B](t: Tree[A])(z: B)(f: (B,A) => B): B = ???
  
  override def foldMap[A,B](t: Tree[A])(f: A => B)(m: Monoid[B]): B = ???
}

object MonoidDriver extends App {
  val foldable01 = new FoldableList
  val xs01 = List(1,2)
  val rs01 = foldable01.foldMap(xs01)(a => a + 1)(intProductMonoid)
  println(rs01)
  
  //목록단어들의 모든 문자의 길이를구하여라
  val xs = List("abcd","efgd","ab")
  val rs = listFoldMap(xs)(x => x.length)(intAddMonoid)
  println(rs)
  
  val xs02 = List(1,2,3,4,5)
  val rs12 = foldLeftViaFoldMap(xs02)(stringMonoid.zero)((b,a) => a.toString + b)
  println(rs12)
  
  val rs11 = foldRightViaFoldMap(xs02)(stringMonoid.zero)((a,b) => a.toString + b)
  
  //ex-13)
  val xs13 = IndexedSeq(1,2,3,4,5) 
  val rs13 = foldMapV(xs13,stringMonoid)(x => x.toString)
  println(rs13)
  
  //ex-14)
  val rs14 = parFoldMapV(xs13,stringMonoid)(x => x.toString())
  val es = Executors.newFixedThreadPool(5)
  val time1 = System.nanoTime()
  println(rs14(es))
  val time2 = System.nanoTime()
  println(time2 - time1)
  println("###")
  
  //ex-15)
  val xs14 = IndexedSeq(12,3,5,4)
  println(ordered(xs14))
  
  //ex-21)
  val o0801 = Some(Some(2))
  val o0802 = Some(Some(3))
  val rx0801 = mergeOption(mergeOption(intAddMonoid)).op(o0801, o0802)
  println(rx0801)
  
  //ex-22)
  val m1:Map[String,Map[String,Int]] = Map("map" -> Map("a" -> 1, "b" -> 2))
  val m2:Map[String,Map[String,Int]] = Map("map" -> Map("b" -> 3, "c" -> 2))
  
  val mergeM: Monoid[Map[String,Map[String,Int]]] = mergeMapMonoid(mergeMapMonoid(intAddMonoid))
  val rs08: Map[String,Map[String,Int]] = mergeM.op(m1,m2)
  println(rs08)
  
  //ex-23)
  val mo01 = Some(Map("map" -> Map("a" -> 1, "b" -> 2)))
  val mo02 = Some(Map("map" -> Map("b" -> 3, "c" -> 2)))
  
  val mMonoid2:Monoid[Option[Map[String,Map[String,Int]]]] = mergeOption(mergeMapMonoid(mergeMapMonoid(intAddMonoid)))
  val rs0803 = mMonoid2.op(mo01, mo02)
  println(rs0803)
  
  //ex-24
  val xs09 = List(1,2,3,4,5,6,7,8,9,10)
  val m09 = productMonoid(intAddMonoid,intAddMonoid)
  val listFoldable = new FoldableList
  val rs09 = listFoldable.foldMap(xs09)(a => (1,a))(m09)
  
  println(rs09)
  
  println(bag(Vector("a","rose","is","a","rose")))
}

