package basic.monoid

import java.util.concurrent.Executors

trait Study {
  
  //monoid 는 하나의 형식(Monoid[A] 의 A)이되, 그 형식(A)에 대해 결합법칙을 만족하며 항등(zero)을
  //가진 이항연산(op)이 존재하는 형식이다.
  //Monoid[A] 의 instance는 이 monoid 를 증명(증거)일 뿐이다.
}

//monoid law = 항등원의 법칙, 결합법칙
//monoid 구성
// 어떤 형식 A
// 그 형식 2개를 하나의 값을 산출하는 항등법칙과 결합법칙이 성립하는 이항연산
// 항등원
trait Monoid[A] {

  //op(op(a1,a2),a3) == op(a1,op(a2,a3))
  def op(a1: A, a2: A): A = ???

  //op(a1,zero) == op(zero,a1) == a1
  def zero: A = ???
}

object StudyDriver extends App {
  
  val stringMonoid = new Monoid[String] {
    override def op(s1: String, s2: String): String = s1 + s2
    override def zero: String = ""
  }
  
  val listMonoid = new Monoid[List[Int]] {
    override def op(xs: List[Int], ys: List[Int]): List[Int] = xs ++ ys
    override def zero: List[Int] = Nil
  }
  
  //ex-01)정수 덧셈과 곱셈에 대한 Monoid instance들과 해당 Boolean 연산자들을 제시하라.
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }
  
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }
  
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }
  
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }
  
  //ex-02)Option[String] 에 대한 Monoid를 작성하라.
  val optionMonoid = new Monoid[Option[String]] {
    override def op(o1: Option[String], o2: Option[String]): Option[String] = o1 orElse o2
    override def zero: Option[String] = None
  }
  
  val o1 = Some("a")
  val o2 = Some("b")
  val o3 = Some("c")
  //Option[String] 에 대한 결합법칙 
  val rs01 = optionMonoid.op(o1,optionMonoid.op(o2,o3))
  val rs02 = optionMonoid.op(optionMonoid.op(o1,o2),o3)
  println("rs01 :"+ rs01+ " rs02:"+rs02)
  
  //Option[String]에 대한 항등원법칙 
  val rs03 = optionMonoid.op(o1,optionMonoid.zero)
  val rs04 = optionMonoid.op(optionMonoid.zero,o1)
  println("rs03 :"+ rs03+ " rs03:"+rs04)
  
  //ex-03)인수의 형식과 반환값의 형식이 같은 함수를 자기함수(endofunction)이라 한다. 자기함수들을 위한 monoid를 작성하라.
  def endoM[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(f1: A => A, f2: A => A): A => A = f1 compose f2
    override def zero: A => A = a => a
  }
  //endoMonoid Test
  //1 결합법칙 
  val endoMonoid = endoM[Int]
  val rs05 = endoMonoid.op(x => x + x, endoMonoid.op(y => y * y, z => z - 1))(3)
  val rs06 = endoMonoid.op(endoMonoid.op(x => x + x, y => y * y), z => z - 1)(3)
  println("rs05 :"+ rs05+ " rs06:"+rs06)
  
  //2.항등원법칙
  val rs07 = endoMonoid.op(x => x + x, endoMonoid.zero)(3)
  val rs08 = endoMonoid.op(endoMonoid.zero, x => x + x)(3)
  println("rs07 :"+ rs07+ " rs07:"+rs07)
  
  //List의 foldRight 와 foldLeft에 대한 고찰
  val xs = List("ss","lee"," study"," scala")
  val rs09 = xs.foldLeft(stringMonoid.zero)(stringMonoid.op)
  println(rs09)
  
  // List의 목록 형식이 Monoid instance의 형식과 부합하지 않는다면 map를 이용하면 된다.
  //ex-04) foldMap를 구현하라.
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = 
    as.foldLeft(m.zero)((b,a) => {
      println(b+":"+a)
      m.op(b,f(a))
    }) // b(f(a))
  
  val xs02 = List(1,2,3,4,5)
  val rs10 = foldMap(xs02,stringMonoid)(x => x.toString)
  println(rs10)
  
  //ex-05 foldRight 을 foldMap으로 구현하라.
  //f:(A,B) => B 는 f.curried : A => (B => B) 여기서 B => B type은 호출하고 있는 foldMap의 또 다른 B type 이다.
  // 즉 type B[B] = B => B 
  // foldMap의 B type을 B => B 로 생각하면 됨.
  def foldRightViaFoldMap[A,B](xs:List[A])(z:B)(f:(A,B) => B): B = {
		  val t = foldMap(xs,endoM[B])(f.curried)
      t(z)
  }
  
  // foldMap 은 다음과 같이 진행 (foldMap 안에서 endoMonoid에서 g(f(x))를 하고 있고 
  // foldLeft를 사용하고 있다. 따라서 head 쪽에서 tail쪽으로 f(x)가 g(x) 로 induction 되므로 head 쪽에서 tail쪽을 받는 형태가 된다. 
  // f(1) => (x => 1.toString + x)  result: 1.toString + x
  // f(2) => (x => 2.toString + x)  result: 1.toString + (x => 2.toString + x)
  // f(3) => (x => 3.toString + x)  result: 1.toString + 2.toString + (x => 3.toString + x)
  // f(4) => (x => 4.toString + x)  result: 1.toString + 2.toString + 3.toString + (x => 4.toString + x)
  // f(5) => (x => 5.toString + x)  result: 1.toString + 2.toString + 3.toString + 4.toString + (x => 5.toString + x)
  // 이후 endoMonoid op 에서  x => x 
  val rs11 = foldRightViaFoldMap(xs02)(stringMonoid.zero)((a,b) => {
    println("##"+a+":"+ b)
    a.toString + b
  })
  println("#########")
  println(rs11)
  
  //ex-06 foldLeft을 foldMap으로 구현하라.
  def dualMonoid[A](m: Monoid[A]) = new Monoid[A] {
    override def op(f1: A, f2: A) = m.op(f2,f1)
    override def zero: A = m.zero
  }
  
  def foldLeftViaFoldMap[A,B](xs: List[A])(z:B)(f: (B,A) => B): B = 
    foldMap(xs,dualMonoid(endoM[B]))(a => b => f(b,a))(z)
  
  // foldMap 은 다음과 같이 진행 foldLeft를 사용하고 있다. 
  // 따라서 head 쪽에서 tail쪽으로 f(x)가 g(x) 로 induction 되어야 할 것 같지만 dualMonoid에서 
  // f(g(x))를 하고 있어 (정의역과 치역이 뒤바뀐다.) tail 쪽에서 header쪽으로 g(x)가 f(x)를 받고 있다.
  //   공역(X)                           정의역(Y)
  // f(1) => (x => 1.toString + x)  result: 1.toString + x
  // f(2) => (x => 2.toString + x)  result: 2.toString + (x => 1.toString + x) 
  // f(3) => (x => 3.toString + x)  result: 3.toString + (x => 2.toString + 1.toString + x)
  // f(4) => (x => 4.toString + x)  result: 4.toString + (x => 3.toString + 2.toString + 1.toString + x)
  // f(5) => (x => 5.toString + x)  result: 5.toString + (x => 4.toString + 3.toString + 2.toString + 1.toString + x)
  val rs12 = foldLeftViaFoldMap(xs02)(stringMonoid.zero)((b,a) => {
    println("##"+a+":"+ b)
    a.toString + b
  })
  println(rs12)
  
  //ex-07)IndexedSeq에 대한 foldMap을 구현하라. 구현은 반드시 순차열을 둘로 분할해서  
  //재귀적으로 각 절방을 처리하고 그결과들을 monoid를 이용해서 결합해야 한다.
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if(v.isEmpty) m.zero
    else if(v.size == 1) f(v(0))
    else {
      //println(v.size)
      val (l,r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l,m)(f), foldMapV(r,m)(f))
      
    }
  }
  
  val xs13 = IndexedSeq(1,2,3,4,5) 
  val rs13 = foldMapV(xs13,stringMonoid)(x => x.toString)
  println(rs13)
  
  //ex-08) 순차열은 균등분할 하여 병렬처리한는 parFoldMap를 구현하라.
  //A => Par[A] 로 승격함수를 만들어야 한다.
  import basic.parallel.Par._
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(p1: Par[A], p2: Par[A]): Par[A] = p1.map2(p2)(m.op)
    override def zero: Par[A] = unit(m.zero)
  }
  
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    foldMapV(v,par(m))(asyncF(f))
    
//    val r: Par[IndexedSeq[B]] = parMap(v)(f)
//    r.flatMap(seqB => foldMapV(seqB,par(m))(b => lazyUnit(b)) )
  }
  
  val rs14 = parFoldMap(xs13,stringMonoid)(x => x.toString())
  val es = Executors.newFixedThreadPool(5)
  val time1 = System.nanoTime()
  println(rs14(es))
  val time2 = System.nanoTime()
  println(time2 - time1)
  println("###")
  
  //ex-09) 순차열을 균등분할 하여  정렬되어있는지 판단하는 ordered 를 구현하라.
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
  
  val xs14 = IndexedSeq(12,3,5,4)
  println(ordered(xs14))
  
  //ex-10) 단어를 새는 wordCount를 만들어라.
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC
  
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def zero = new Stub("")
    override def op(op1: WC, op2: WC): WC = (op1,op2) match {
      case (Stub(a),Stub(b)) => Stub(a + b)
      case (Stub(a), Part(l,i,r)) => Part(a + l, i, r)
      case (Part(l,i,r), Stub(a)) => Part(l, i, r + a)
      case (Part(l1,i1,r1), Part(l2,i2,r2)) => Part(l1, i1 + (if( (r1 + l2).isEmpty) 0 else 1) + i2, r2)  
    }
  }
  
  def wordCount(xs: String): Int = {
    
    def wc(c: Char): WC = {
      if(c.isWhitespace) Part("", 0, "")
      else Stub(c.toString())
    }
    
    def min(x: String): Int = x.length min 1
    
    foldMapV(xs, wcMonoid)(wc) match {
      case Stub(x) => min(x)
      case Part(l,i,r) => min(l) + i + min(r) 
    }
  }
  
  println("############")
  val xs15 = "scala is not easy but I will not git up"
  println(wordCount(xs15))
  
  /**
   대수란: 정수나 유리수와 같이 우리에게 익숙한 수들의 기본 성질들을 추상화, 일반화하는 것을 포함하는 아이디어와 기능으로 구성된 
   		   수학적 세계 중에 하나.
   대수학: 정수가 가지고 있는 성질의 일반화에 대한 연구이다.
   집합 : 조건이 주어졌을때, 그 조건을 가르키는 만족하는 분명한 것들의 모임.
   군   : 결합법칙,항등법칙,교환법칙이 성립하는 성질을 임의의 집합으로 일반화 한 것이 군이다.
   사상 : 대상의 관계에 대한 mapping, 혹은 function ( 임이의 군 => 임이의 군 )
   
   ✶를 하나의 집합 G에 대한 이항 연산이라 한다면( 임이의 x,y ∈ G => x ✶ y ∈ G )
   다음의 성질을 만족하면 G 또는 (G,✶)를 군이라 한다.
   
   l-1) x,y,z ∈ G => x ✶ (y ✶ z) = (x ✶ y) ✶ z)   : ✶에 대한 결합법칙 
   l-2) 모든 x ∈ G 에 대하여 e ✶ x = x ✶ e = x 을 만족하는 e ∈ G 가 존재한다. : 항등원과 ✶에 대한 항등법칙 
   l-3) 각각의 x ∈ G 에 대하 x^-1 ∈ G 가 존재하며 x ✶ x^-1 = x^-1 ✶ x = e (x는 역원를 갖는다.) : 역원 
   
   추가로 다음을 만족하면 가환군(아벨군)이라 한다.
   l-4) x,y ∈ G => x ✶ y = y ✶ x   : ✶ 에 대한 교환법칙
   
   monoid: ✶를 하나의 집합 G에 대한 이항 연산이라 한다면( 임이의 x,y ∈ G => x ✶ y ∈ G ) l-1,l-2,l-3의 성질을 가지는 군을 monoid라 한다.
   Monoid[A] {
   	def op(a: A, b: A): A = ???( 결합법칙 이 성립하게 구현)
   	def zero: A = ??? (항등원)
   }
   A 또는 Monoid[A]의 인스턴스를 monoid 라 한다.
   
   homomorphism( 준동형 사상) 
   String의 length function(사상) 은 String => Int 
   "hellow".length + "scala".length = ("hellow" + "scala").length
   Monoid M,n 에 대하여 
   M.op(f(x),f(y)) = f(N.op(x,y)) 를 만족하는 f는  준동형 사상 함수이다.
   
   f: length
   M: Int
   N: String 
   op: + 
   
   두 monoid에서 준동형사상은 한쪽의 monoid으로 적용된며 역은 되지 않는다 역  위의 식에서 M과 N을 각각 String,Int로 바꾸어 생각해보면 알 수 있음.
   두 monoid M,N 에 준 동형사상이 양방향으로 존재할 경우  각각의 준동형사상 함수가 존재 하며 이를 각각 f, g라 할때 
   g(f) = f(g) 는 모두 항등사상이다.
   */
  
  //################ 접기 자료구조 형식의 일반화 ####################
  //ex-11) 접기자료구조의 일반화를 구현하라.
  trait Foldable[F[_]] {
    def foldRight[A,B](xs: F[A])(z: B)(f: (A,B) => B): B =
      foldMap(xs)(f.curried)(endoM[B])(z)
      
    def foldLeft[A,B](xs: F[A])(z: B)(f: (B,A) => B): B = 
      foldMap(xs)(a => (b:B) => f(b,a))(dualMonoid(endoM))(z)
      
    def foldMap[A,B](xs: F[A])(f: A => B)(m: Monoid[B]): B = 
      foldRight(xs)(m.zero)((a,b) => m.op(f(a),b))
      
    def concat[A](xs: F[A])(m: Monoid[A]): A = 
      foldLeft(xs)(m.zero)(m.op) 
      
    def toList[A](s: F[A]): List[A] = 
      foldRight(s)(List[A]())((a,b) => a::b)
  }
  
  //F[_] 은 어떤 형식이아닌 _ 의 임이의 형식을 담는 형식생성자를 뜻함. 이를 high-order type constructor라 한다.
  //List[A] 나 Option[A] 등 처럼.
  
  //ex-12)Foldable[List]를 구현하라.
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
  
  //ex-13) Foldable[IndexedSeq] 구현하라.
  //foldRight,foldLeft,foldMap 를 구현하라.
  class FoldableIndexedSeq extends Foldable[IndexedSeq]  {
    override def foldRight[A,B](xs: IndexedSeq[A])(z: B)(f: (A,B) => B): B = 
      xs.foldRight(z)(f)
      
    override def foldLeft[A,B](xs: IndexedSeq[A])(z: B)(f: (B,A) => B): B = 
      xs.foldLeft(z)(f)
      
    override def foldMap[A,B](xs: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B = 
      foldMapV(xs,m)(f)
  }
  
  //ex-14) Foldable[Stream]를 구현하라.
  //foldRight, foldLeft를 구현하라.
  class FoldableStream extends Foldable[Stream] {
    override def foldRight[A,B](xs: Stream[A])(z: B)(f: (A,B) => B): B = 
      xs.foldRight(z)(f)
    
    override def foldLeft[A,B](xs: Stream[A])(z: B)(f: (B,A) => B): B = 
      xs.foldLeft(z)(f)
  }
  
  //ex-15) 다음의 이진 Tree 자료형식에 대한 Foldable[Tree]에 foldRight,foldLeft,foldMap  구현하라.
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
  
  //ex-16) Foldable[Option] foldRight,foldLeft,foldMap를 override한 인스턴스를 구현하라.
  class FoldableOption extends Foldable[Option] {
    override def foldRight[A,B](o: Option[A])(z: B)(f: (A,B) => B): B = o match {
      case None => z
      case Some(a) => f(a,z)
    }
    
    override def foldLeft[A,B](o: Option[A])(z: B)(f: (B,A) => B): B = o match {
      case None => z
      case Some(a) => f(z,a)
    }
    
    override def foldMap[A,B](o: Option[A])(f: A => B)(m: Monoid[B]): B = 
      foldRight(o)(m.zero)((a,b) => m.op(f(a),b))
  }
  
  //ex-17) monoid 의 합성 중 다음의 두 monoid의 곱을 구현하라.
  // 곱의 표현은 (A,B) 로 한다.
  def productMonoid[A,B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)] {
    override def op(a: (A,B), b: (A,B)): (A,B) = (ma.op(a._1,b._1), mb.op(a._2, b._2))
    override def zero:(A,B) = (ma.zero,mb.zero)
  }
  
  def mapMergeMonoid[K,V](vm: Monoid[V]): Monoid[Map[K,V]] = new Monoid[Map[K,V]] {
    override def zero = Map[K,V]()
    override def op(ma: Map[K,V], mb: Map[K,V]): Map[K,V] = {
      (ma.keySet ++ mb.keySet).foldLeft(zero)( (acc,k) => 
        acc.updated(k, vm.op(ma.getOrElse(k,vm.zero), mb.getOrElse(k,vm.zero) ) ) )
    }
  }
  
  val m1 = Map("o1" -> Map("l1" -> 1, "i2" -> 2))
  val m2 = Map("o1" -> Map("i2" -> 3))
  val m: Monoid[Map[String,Map[String,Int]]] = mapMergeMonoid(mapMergeMonoid(intAddition))
  val rs18 = m.op(m1, m2)
  println("map merge=>"+rs18)
  
  def optionMergeMonoid[S](m: Monoid[S]): Monoid[Option[S]] = new Monoid[Option[S]] {
    override def zero = None
    override def op(o1: Option[S], o2: Option[S]): Option[S] = {
      Some(m.op(o1.getOrElse(m.zero), o2.getOrElse(m.zero)))
    }
  }
  
  val oa19 = Some(Some(2))
  val ob19 = Some(Some(3))
  val m19: Monoid[Option[Option[Int]]] = optionMergeMonoid(optionMergeMonoid(intAddition))
  val rs19 = m19.op(oa19,ob19)
  println(rs19)
  
  //ex-20) 결과가 monoid 인 함수들에 대한 monoid instance를 작성하라.
  def functionMonoid[A,B](m: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def zero: A => B = a => m.zero
    override def op(f1: A => B, f2: A => B): A => B = a => m.op(f1(a),f2(a))
  }
  
  //ex-21) bag라는 자료구조는 집합처럼 각 요소를 하나씩만 담되, 그 요소의 출현 횟수도 기억한다.
  //구체적으로, 자루는 각 요소가 key 이고 그 요소의 출현 횟수가 값인 Map[요소,출현횟수] 으로 표현된다.
  // bag(Vector("a","rose","is","a","rose")) 일때 결과는 
  // res0: Map[String,Int] = Map(a -> 2, is -> 1, rose -> 2)  가 나오도록 
  // monoid들을 이용해서 IndexedSeq 로 부터 bag 를 구현하라.
  def bag[A](xs: IndexedSeq[A]): Map[A,Int] = 
    foldMapV(xs,mapMergeMonoid[A,Int](intAddition))(x => Map(x -> 1))
  
 
  val xs21 = IndexedSeq("a","rose","is","a","rose")
  println(bag(xs21))
  
  
  
}