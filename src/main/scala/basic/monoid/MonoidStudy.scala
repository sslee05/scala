package basic.monoid

import java.util.concurrent.Executors

object MonoidStudy {
  
  trait Monoid[A] {
    def op(a: A, b: A): A = ???
    def zero: A = ???
  }
  
  //ex-01) String + 연산에 대한 Monoid를 만들어 보자. 
  def stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a: String, b: String): String = a + b
    override def zero: String = ""
  }
  
  //ex-02) Int 덧셈에 대한 Monoid를 만들어 보자.
  def intAddMonoid: Monoid[Int] = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a + b
    override def zero: Int = 0
  }
  
  //ex-03)Boooean or 연산에 대한 Monoid를 만들어 보자.
  def booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = false
    override def op(a: Boolean, b: Boolean) = a || b
  }
  
  //ex-04)Boooean or 연산에 대한 Monoid를 만들어 보자.
  def booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def zero: Boolean = true
    override def op(a: Boolean, b: Boolean) = a && b
  }
  
  //ex-05) Int 곱셈에 대한 Monoid를 만들어 보자.
  def intProductMonoid: Monoid[Int] = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a * b
    override def zero: Int = 1
  }
  
  //ex-06) Option 조합 연산에 대한 Monoid를 만들어라.
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(o1: Option[A], o2: Option[A]): Option[A] = o1.orElse(o2)
    override def zero: Option[A] = None
  }
  
  //ex-07) 자기함수: 인수의 형식과 반환값의 형식이 동일한 함수 
  // 자기함수 monoid를 만들어 보자.
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(f: A => A, g: A => A): A => A = f compose g
    override def zero: A => A = a => a
  }
  
  //ex-08) 접기함수를 monoid를 이용하여 만들어라.
  def fold[A](xs: List[A])(m: Monoid[A]): A = 
    xs.foldRight(m.zero)((a,b) => m.op(a, b))
    
  //ex-09) 시작과 결과의 형식이 다른 목록의 접기 함수를 monoid를 사용하여 작성하라.
  def foldMap[A,B](xs: List[A])(f: A => B)(m: Monoid[B]): B = 
    xs.foldRight(m.zero)((a,b) => m.op(f(a),b))
  
  //ex-10)
  // foldRight를 foldMap를 이용해서 구현하라.
  //f:(A,B) => B 는 f.curried : A => (B => B) 여기서 B => B type은 호출하고 있는 foldMap의 또 다른 B type 이다.
  // 즉 type B[B] = B => B 
  // foldMap의 B type을 B => B 로 생각하면 됨.
  def foldRightViaFoldMap[A,B](xs:List[A])(z:B)(f:(A,B) => B): B = {
		  val t = foldMap(xs)(f.curried)(endoMonoid)
      t(z)
  }
  
  //ex-11) foldLeft을 foldMap으로 구현하라.
  def dualMonoid[A](m: Monoid[A]) = new Monoid[A] {
    override def op(f1: A, f2: A) = m.op(f2,f1)
    override def zero: A = m.zero
  }
  
  //ex-12)
  def foldLeftViaFoldMap[A,B](xs: List[A])(z:B)(f: (B,A) => B): B = 
    foldMap(xs)(a => (b:B) => f(b,a))(dualMonoid(endoMonoid[B]))(z)
  
  //ex-13)IndexedSeq에 대한 foldMap을 구현하라. 구현은 반드시 순차열을 둘로 분할해서  
  //재귀적으로 각 절방을 처리하고 그결과들을 monoid를 이용해서 결합해야 한다.
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if(v.isEmpty) m.zero
    else if(v.size == 1) f(v(0))
    else {
      val (l,r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l,m)(f), foldMapV(r,m)(f))
      
    }
  }
  
  //ex-14) 순차열은 균등분할 하여 병렬처리한는 parFoldMap를 구현하라.
  //A => Par[A] 로 승격함수를 만들어야 한다.
  import basic.parallel.Par._
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(p1: Par[A], p2: Par[A]): Par[A] = p1.map2(p2)(m.op)
    override def zero: Par[A] = unit(m.zero)
  }
  
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    foldMapV(v,par(m))(asyncF(f))
  
  //ex-15) 순차열을 균등분할 하여  정렬되어있는지 판단하는 ordered 를 구현하라.
  def ordered(xs: IndexedSeq[Int]): Boolean = {
    val m = new Monoid[Option[(Int,Int,Boolean)]] {
      override def zero = None
      override def op(op1: Option[(Int,Int,Boolean)], op2: Option[(Int,Int,Boolean)]) : Option[(Int,Int,Boolean)] = {
        (op1,op2) match {
          case (None,x) => x
          case (x,None) => x
          case (Some((x,y,z)),Some((x1,y1,z1))) => 
            Some((x min x1, y max y1 , z && z1 && y <= x1))
        }
      }
    }
    
    foldMapV(xs,m)(x => Some(x,x,true)).map(x => x._3).getOrElse(true)
  }
  
  
  //ex-16)fold 의 일반화 
  trait Foldable[F[_]] {
    def foldRight[A,B](xs: F[A])(z: B)(f: (A,B) => B): B =
      foldMap(xs)(f.curried)(endoMonoid[B])(z)
      
    def foldLeft[A,B](xs: F[A])(z: B)(f: (B,A) => B): B = 
      foldMap(xs)(a => (b:B) => f(b,a))(dualMonoid(endoMonoid))(z)
      
    def foldMap[A,B](xs: F[A])(f: A => B)(m: Monoid[B]): B = 
      foldRight(xs)(m.zero)((a,b) => m.op(f(a),b))
      
    def concat[A](xs: F[A])(m: Monoid[A]): A = 
      foldLeft(xs)(m.zero)(m.op) 
      
    def toList[A](s: F[A]): List[A] = 
      foldRight(s)(List[A]())((a,b) => a::b)
  }
  
  //ex-17)Foldable[List]를 구현하라.
  //foldRight,foldLeft,foldMap, toList를 구현하라.
  class FoldableList extends Foldable[List] {
    override def foldRight[A,B](xs: List[A])(z: B)(f: (A,B) => B): B = 
      xs.foldRight(z)(f)
    
    override def foldLeft[A,B](xs: List[A])(z: B)(f: (B,A) =>B): B = 
      xs.foldLeft(z)(f)
   
    override def foldMap[A,B](xs: List[A])(f: A => B)(m: Monoid[B]): B = 
      foldLeft(xs)(m.zero)((b,a) => m.op(b, f(a)))
      
    override def toList[A](xs: List[A]): List[A] = xs  
  }
  
  //ex-18) Foldable[IndexedSeq] 구현하라.
  //foldRight,foldLeft,foldMap 를 구현하라.
  class FoldableIndexedSeq extends Foldable[IndexedSeq]  {
    override def foldRight[A,B](xs: IndexedSeq[A])(z: B)(f: (A,B) => B): B = 
      xs.foldRight(z)(f)
      
    override def foldLeft[A,B](xs: IndexedSeq[A])(z: B)(f: (B,A) => B): B = 
      xs.foldLeft(z)(f)
      
    override def foldMap[A,B](xs: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B = 
      foldMapV(xs,m)(f)
  }
  
  //ex-19) Foldable[Stream]를 구현하라.
  //foldRight, foldLeft를 구현하라.
  class FoldableStream extends Foldable[Stream] {
    override def foldRight[A,B](xs: Stream[A])(z: B)(f: (A,B) => B): B = 
      xs.foldRight(z)(f)
    
    override def foldLeft[A,B](xs: Stream[A])(z: B)(f: (B,A) => B): B = 
      xs.foldLeft(z)(f)
  }
  
  //ex-20) 다음의 이진 Tree 자료형식에 대한 Foldable[Tree]에 foldRight,foldLeft,foldMap  구현하라.
  sealed trait Tree[+A]
  case class Leaf[A](v: A) extends Tree[A]
  case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]
  
  class FoldableTree extends Foldable[Tree] {
    override def foldRight[A,B](t: Tree[A])(z: B)(f: (A,B) => B) : B = t match {
      case Leaf(v) => f(v,z)
      case Branch(l,r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
    
    override def foldLeft[A,B](t: Tree[A])(z: B)(f: (B,A) => B): B = t match {
      case Leaf(v) => f(z,v)
      case Branch(l,r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
    
    override def foldMap[A,B](t: Tree[A])(f: A => B)(m: Monoid[B]): B = 
      foldRight(t)(m.zero)((a,b) => m.op(f(a),b))
  }
  
  //ex-21)monoid의 합성
  def mergeOption[O](m: Monoid[O]): Monoid[Option[O]] = {
      new Monoid[Option[O]] {
        override def zero: Option[O] = None
        override def op(o1: Option[O], o2: Option[O]): Option[O] = 
          o1 flatMap(a => o2 map(b => m.op(a, b)))
      }
  }
  
  //ex-22)map merge
  def mergeMapMonoid[K,V](m: Monoid[V]): Monoid[Map[K,V]] = {
    new Monoid[Map[K,V]] {
      override def zero: Map[K,V] = Map[K,V]()
      override def op(ma: Map[K,V], mb: Map[K,V]): Map[K,V] = 
        (ma.keySet ++ mb.keySet).foldLeft(zero) { (b1,a1) => 
          b1.updated(a1,m.op(ma.getOrElse(a1,m.zero), mb.getOrElse(a1,m.zero)))
        }   
    }
  }
  
  //ex-23)monoid 곱을 구현하라.
  def productMonoid[A,B](m1: Monoid[A], m2: Monoid[B]): Monoid[(A,B)] = 
    new Monoid[(A,B)] {
      override def zero: (A,B) = (m1.zero,m2.zero)
      override def op(a: (A,B) , b: (A,B)): (A,B) = (m1.op(a._1,b._1),m2.op(a._2,b._2))
    }
  
  //ex-24)목록의 길이와 원소의 합을 동시에 구할 수 있다.
  //밑에 MonoidStudyDriver 에 
  
  //ex-25) 결과가 monoid 인 함수들에 대한 monoid instance를 작성하라.
  def functionMonoid[A,B](m: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def zero: A => B = a => m.zero
    override def op(f1: A => B, f2: A => B): A => B = a => m.op(f1(a),f2(a))
  }
  
  //ex-26) bag라는 자료구조는 집합처럼 각 요소를 하나씩만 담되, 그 요소의 출현 횟수도 기억한다.
  //구체적으로, 자루는 각 요소가 key 이고 그 요소의 출현 횟수가 값인 Map[요소,출현횟수] 으로 표현된다.
  // bag(Vector("a","rose","is","a","rose")) 일때 결과는 
  // res0: Map[String,Int] = Map(a -> 2, is -> 1, rose -> 2)  가 나오도록 
  // monoid들을 이용해서 IndexedSeq 로 부터 bag 를 구현하라.
  def bag[A](xs: IndexedSeq[A]): Map[A,Int] = 
    foldMapV(xs,mergeMapMonoid[A,Int](intAddMonoid))(x => Map(x -> 1))
     
}


object MonoidStudyDriver extends App {
  
  import basic.monoid.MonoidStudy._
  
  //목록단어들의 모든 문자의 길이를구하여라
  val xs = List("abcd","efgd","ab")
  val rs = foldMap(xs)(x => x.length)(intAddMonoid)
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
  val rs14 = parFoldMap(xs13,stringMonoid)(x => x.toString())
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
  val rs09 = foldMap(xs09)(a => (1,a))(m09)
  
  println(rs09)
}