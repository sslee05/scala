package exercise.monoid

trait Monoid[A] {

  def zero: A
  def op(a: A, b: A): A
  
}

object Monoid {

  val stringMonoid: Monoid[String] = ???

  val intAddMonoid: Monoid[Int] = ???

  val intProductMonoid: Monoid[Int] = ???

  val booleanOrMonoid: Monoid[Boolean] = ???
  
  val booleanAndMonoid: Monoid[Boolean] = ???

  def optionMonoid[A]: Monoid[Option[A]] = ???

  def endoMonoid[A]: Monoid[A => A] = ???
  
  //ex-08)list에 대한 접기 함수를 monoid를 이용하여 구현
  def listFold[A](xs: List[A])(m: Monoid[A]): A = ???
    
  //ex-09) List에 대한 시작과 결과의 형식이 다른 목록의 접기 함수를 monoid를 사용하여 작성하라.
  def listFoldMap[A,B](xs: List[A])(f: A => B)(m: Monoid[B]): B = ???
    
  //ex-10) List에 대한 listFoldMap 함수를 foldRight를 이용하여 구현하라
  // foldRight를 foldMap를 이용해서 구현하라.
  //f:(A,B) => B 는 f.curried : A => (B => B) 여기서 B => B type은 호출하고 있는 foldMap의 또 다른 B type 이다.
  // 즉 type B[B] = B => B 
  // foldMap의 B type을 B => B 로 생각하면 됨.
  def foldRightViaFoldMap[A,B](xs:List[A])(z:B)(f:(A,B) => B): B = ???
  
  //ex-11) foldLeft를 listFoldMap을 이용해서 구현하라
  //정의역과 치역을 바꾸는 함수가 필요하다. 이를 구현 하라. 
  def dualMonoid[A](m: Monoid[A]) = ???
  
  def foldLeftViaFoldMap[A,B](xs: List[A])(z:B)(f: (B,A) => B): B = ???
  
  //ex-12) IndexedSeq에 대한 listFoldMap를 구현하라.
  //구현은 반드시 순차열을 둘로 분할해서 재귀적으로 각 절반을 처리하고 그 결과들을 monoid를 이용해서 결합해야 한다.
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = ???
  
  //ex-13) foldMapV의 병렬버전도 구현해라.
  //parallel chapter에서 foldMap를 만들었다 이를 monoid를 이용해서 구현하라. 이때  
  //승격 함수인 Monoid[A] => Monoid[Par[A]] 가 필요하다.
  import basic.parallel.Par._
  def par[A](m: Monoid[A]): Monoid[Par[A]] = ???
  
  def parFoldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = ???
  
  //ex-14)foldMap를 이용해서 주어진 IndexedSeq[Int] 가 정렬되어 있는지 점검하라.
  //Monoid를 이용할 것
  def ordered(xs: IndexedSeq[Int]): Boolean = ???
  
  //ex-20)2개의 Option(Option(Int)) 의 자료구조의 병합을 위의 monoide들을 합성하여 구현하라.
  def mergeOption[O](m: Monoid[O]): Monoid[Option[O]] = ???
  
  //ex-21)2개의 Map[String,Map[String,Int]] 의 자료구조의 병합을 위의 monoide들을 합성하여 구현하라
  def mergeMapMonoid[K,V](m: Monoid[V]): Monoid[Map[K,V]] = ???
  
  //ex-22) 2개의 Option[Map[String,Map[String,Int]]] 의 자료구조의 병합을 위의 2문제를 이용하여 작성하라.
  val mMonoid2: Monoid[Option[Map[String,Map[String,Int]]]]= ???
 
  //ex-23)monoid 곱을 구현하라.
  def productMonoid[A,B](m1: Monoid[A], m2: Monoid[B]): Monoid[(A,B)] = ???
  
  //ex-24)결과가 monoid 인 함수들에 대한 monoid instance를 작성하라.
  def functionMonoid[A,B](m: Monoid[B]): Monoid[A => B] = ???
  
  //ex-25) bag라는 자료구조는 집합처럼 각 요소를 하나씩만 담되, 그 요소의 출현 횟수도 기억한다.
  //구체적으로, 자루는 각 요소가 key 이고 그 요소의 출현 횟수가 값인 Map[요소,출현횟수] 으로 표현된다.
  // bag(Vector("a","rose","is","a","rose")) 일때 결과는 
  // res0: Map[String,Int] = Map(a -> 2, is -> 1, rose -> 2)  가 나오도록 
  // monoid들을 이용해서 IndexedSeq 로 부터 bag 를 구현하라.
  def bag[A](xs: IndexedSeq[A]): Map[A,Int] = ???
    
}

//ex-15)여러 자료구조에서 fold의 공통점을 뽑아 Foldable trait를 구현하라.
import exercise.monoid.Monoid._
import java.util.concurrent.Executors

trait Foldable[F[_]] {
  
  def foldRight[A,B](xs: F[A])(z: B)(f: (A,B) => B): B = ???
    
  def foldLeft[A,B](xs: F[A])(z:B)(f: (B,A) => B): B = ???
  
  def foldMap[A,B](xs: F[A])(f: A => B)(m: Monoid[B]): B = ???
  
  def concat[A](xs: F[A])(m: Monoid[A]): A = ???
   
  def toList[A](xs: F[A]): List[A] = ???
}

//ex-16)
class FoldableList extends Foldable[List] {
  override def foldRight[A,B](xs: List[A])(z: B)(f: (A,B) => B): B = ???
    
  override def foldLeft[A,B](xs: List[A])(z: B)(f: (B,A) =>B): B = ??? 
    
  override def foldMap[A,B](xs: List[A])(f: A => B)(m: Monoid[B]): B = ???
    
  override def toList[A](xs: List[A]): List[A] = ???
}

//ex-17) 다음을 구현하라.
class FoldableIndexedSeq extends Foldable[IndexedSeq]  {
  override def foldRight[A,B](xs: IndexedSeq[A])(z: B)(f: (A,B) => B): B = ???
    
  override def foldLeft[A,B](xs: IndexedSeq[A])(z: B)(f: (B,A) => B): B = ???
    
  override def foldMap[A,B](xs: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B = ???
}

//ex-18) 다음을 구현하라.
class FoldableStream extends Foldable[Stream] {
  override def foldRight[A,B](xs: Stream[A])(z: B)(f: (A,B) => B): B = ???
    
  override def foldLeft[A,B](xs: Stream[A])(z: B)(f: (B,A) => B): B = ???
}

//ex-19) 다음을 구현하라.
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
  //ex-10)
  // foldMap 은 다음과 같이 진행 foldLeft를 사용하고 있다. 
  // 따라서 head 쪽에서 tail쪽으로 f(x)가 g(x) 로 induction 되어야 할 것 같지만 dualMonoid에서 
  // f(g(x))를 하고 있어 (정의역과 치역이 뒤바뀐다.) tail 쪽에서 header쪽으로 g(x)가 f(x)를 받고 있다.
  //   공역(X)                           정의역(Y)
  // f(1) => (x => 1.toString + x)  result: 1.toString + x
  // f(2) => (x => 2.toString + x)  result: 2.toString + (x => 1.toString + x) 
  // f(3) => (x => 3.toString + x)  result: 3.toString + (x => 2.toString + 1.toString + x)
  // f(4) => (x => 4.toString + x)  result: 4.toString + (x => 3.toString + 2.toString + 1.toString + x)
  // f(5) => (x => 5.toString + x)  result: 5.toString + (x => 4.toString + 3.toString + 2.toString + 1.toString + x)
  val rs12 = foldLeftViaFoldMap(xs02)(stringMonoid.zero)((b,a) => a.toString + b)
  println(rs12)
  
  //ex-12)
  // foldMap 은 다음과 같이 진행 (foldMap 안에서 endoMonoid에서 g(f(x))를 하고 있고 
  // foldLeft를 사용하고 있다. 따라서 head 쪽에서 tail쪽으로 f(x)가 g(x) 로 induction 되므로 head 쪽에서 tail쪽을 받는 형태가 된다. 
  // f(1) => (x => 1.toString + x)  result: 1.toString + x
  // f(2) => (x => 2.toString + x)  result: 1.toString + (x => 2.toString + x)
  // f(3) => (x => 3.toString + x)  result: 1.toString + 2.toString + (x => 3.toString + x)
  // f(4) => (x => 4.toString + x)  result: 1.toString + 2.toString + 3.toString + (x => 4.toString + x)
  // f(5) => (x => 5.toString + x)  result: 1.toString + 2.toString + 3.toString + 4.toString + (x => 5.toString + x)
  // 이후 endoMonoid op 에서  x => x 
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
}

