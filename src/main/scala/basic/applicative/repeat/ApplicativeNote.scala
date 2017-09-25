package basic.applicative.repeat

object ApplicativeNote {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  //ex-01) Applicative functor trait를 구현하되 unit 과 apply function을 기본수단으로 하라.
  trait Applicative[F[_]] extends Functor[F] {

    def unit[A](a: => A): F[A]
    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C]

    //ex-02) map를 map2와 unit으로 구현하라.
    def map[A, B](m: F[A])(f: A => B): F[B] =
      map2(m, unit(()))((a, _) => f(a))

    //ex-03) Applicative trait에 traverse 함수를 구현하라.
    def traverse[A, B](xs: List[A])(f: A => F[B]): F[List[B]] =
      xs.foldRight[F[List[B]]](unit(Nil))((a, b) => map2(f(a), b)((a1, b1) => a1 :: b1))

    //ex-04) Applicative trait에 traverse를 이용하여  sequence 함수를 구현하라.
    def sequenceByTraverse[A](xs: List[F[A]]): F[List[A]] =
      traverse(xs)(a => a)

    //ex-05) Applicative trait에 map2 와 unit 으로 sequence 함수를 구현하라.
    def sequence[A](xs: List[F[A]]): F[List[A]] =
      xs.foldRight[F[List[A]]](unit(Nil))((a, g) => map2(a, g)((a1, b1) => a1 :: b1))

    //ex-06) Applicative trait에 map2 와 unit 으로 replicateM 함수를 구현하라.
    def replicateM[A](n: Int, m: F[A]): F[List[A]] =
      if (n <= 0) unit(List[A]()) else map2(m, replicateM(n - 1, m))((a, b) => a :: b)

    //ex-07) Applicative trait에 sequence 으로 replicateM 함수를 구현하라.
    def replicate[A](n: Int, m: F[A]): F[List[A]] =
      sequence(List.fill(n)(m))

    //ex-08) Applicative trait에  map2로 product 함수를 구현하라.
    def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
      map2(ma, mb)((a, b) => (a, b))

    //ex-17) 쌍들의 묶음을 바꾸는 다음의 함수를 구현하라.
    def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) = p match {
      case (a, (b, c)) => ((a, b), c)
    }

    //ex-18)각각의 Input => Output 2개를 받아 Input 쌍을 받고 Outout 쌍을 받는 함수를 구현하라.
    def productF[I1, I2, O1, O2](f: I1 => O1, g: I2 => O2): (I1, I2) => (O1, O2) =
      (i1, i2) => (f(i1), g(i2))

    //ex-12) apply를 map2와 map으로 구현하라.
    def apply[A, B](fo: F[A => B])(m: F[A]): F[B] =
      map2(m, fo)((a, f) => f(a))
      
    //ex-23) Applicative Functor의 합성을 구현하라.
    def compose[G[_]](G: Applicative[G]): Applicative[({type f[X] = F[G[X]]})#f] = {
      val self = this
      new Applicative[({type f[X] = F[G[X]]})#f] {
          def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
          override def map2[A,B,C](ma: F[G[A]], mb: F[G[B]])(f: (A,B) => C): F[G[C]] = {
            self.map2(ma,mb)((a,b) => G.map2(a,b)(f(_,_)))
          }
      }
    }
  }

  //ex-09) 기본수단을 unit과 map2대신 unit과 apply 로 구성하는 적용함자를 구성하라. 
  trait ApplicativeV2[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

    //ex-10)map 를 unit과 apply로 구현하라.
    def map[A, B](m: F[A])(f: A => B): F[B] =
      apply(unit(f))(m)

    //ex-11)map2를 map과 apply로 구현하라.
    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
      apply(map(ma)(f.curried))(mb) // map(ma)(f.curried) 은 F[B => C] 가 return

    //ex-13) map3를 uint,apply,curried 를 이용하여 구현하라.
    def map3[A, B, C, D](ma: F[A], mb: F[B], mc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(apply(unit(f.curried))(ma))(mb))(mc)

    //ex-14) map4를 unit,apply,curried 를 이용하여 구현하라.
    def map4[A, B, C, D, E](ma: F[A], mb: F[B], mc: F[C], md: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(apply(unit(f.curried))(ma))(mb))(mc))(md)

    //ex-22) Monoid 에서 prudct를 통행 2개의 Monid를 받아 두 Monoid를 합처서 1개의 Monoid를 만드므로써 여러기능을 한번에 
    //처리 할 수 있게 했다.
    //그와 같이 Applicative Fuctor에 다음의 함수를 구현하라.
    def productG[G[_]](G: ApplicativeV2[G]): ApplicativeV2[({ type f[X] = (F[X], G[X]) })#f] = {
      val self = this
      new ApplicativeV2[({ type f[X] = (F[X], G[X]) })#f] {
        def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

        override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
          (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))

      }
    }

  }

  object Applicative {
    val optionAF = new Applicative[Option] {
      def unit[A](a: => A): Option[A] = Some(a)
      override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] =
        fa flatMap (a => fab map (f => f(a)))
      def map2[A, B, C](ma: Option[A], mb: Option[B])(f: (A, B) => C): Option[C] =
        apply(map(ma)(f.curried))(mb)
    }

    //ex-15) Either를 위한 Monad instance를 만들어라.
    def eitherMonad[E]: basic.monad.MonadStudy.Monad[({ type f[X] = Either[E, X] })#f] =
      new basic.monad.MonadStudy.Monad[({ type f[X] = Either[E, X] })#f] {
        def unit[A](a: => A): Either[E, A] = Right(a)
        def flatMap[A, B](m: Either[E, A])(f: A => Either[E, B]): Either[E, B] = m match {
          case Right(a) => f(a)
          case Left(e)  => Left(e)
        }
      }

    trait Validation[+E, +A]
    case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
    case class Success[A](a: A) extends Validation[Nothing, A]

    def validator[E] = new Applicative[({ type f[X] = Validation[E, X] })#f] {

      def unit[A](a: => A): Validation[E, A] = Success(a)

      // flatMap으로 구현된 것이 아니기 때문에 mb는 ma와 상관없이 상자에서 풀어 무언가를 할 수 있다.
      def map2[A, B, C](ma: Validation[E, A], mb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (ma, mb) match {
        case (Success(a), Success(b))           => Success(f(a, b))
        case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
        case (e @ Failure(_, _), _)             => e
        case (_, e @ Failure(_, _))             => e
      }
    }
  }

  //ex-02) Monad trait를 만들되 join 함수를 만들고 map,map2를 flatMap으로 구현하라.
  trait Monad[F[_]] extends Applicative[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => unit(f(a)))

    override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      flatMap(fa)(a => map(fb)(b => f(a, b)))
  }

  object Monad {

  }
}

import basic.applicative.repeat.ApplicativeNote._
object ApplicativeNodeDriver extends App {

  val AF = Applicative.optionAF

  val users = Map("sslee" -> "이상석", "iwlee" -> "이일웅", "jkwhang" -> "황진규")
  val lectures = Map("이상석" -> "scala", "이일웅" -> "docker", "황진규" -> "anguler")
  val dates = Map("이상석" -> "Monday", "이일웅" -> "Tuesday", "황진규" -> "Wednesday")

  val rs01 = users get "sslee" flatMap (a => AF.map2(lectures get a, dates get a) { (l, d) =>
    s"$a 님의 강의 과목은 $l 이며 요일은 $d 입니다."
  })

  val rs02 = users get "sslee05" flatMap (a => AF.map2(lectures get a, dates get a) { (l, d) =>
    s"$a 님의 강의 과목은 $l 이며 요일은 $d 입니다."
  })

  println(rs01)
  println(rs02)

  val rs03 = AF.map2(lectures get "이상석", dates get "이상석")((l, d) => {
    s"$l 강의는 $d 요일에 있습니다"
  })
  println(rs03)

  case class User(id: String, name: String, age: Int)
  case class MobilePhone(name: String, os: String)

  def usePhone01(user: Option[User], phone: Option[MobilePhone]): Option[String] =
    AF.map2(user, phone)((u, f) => s"${u.name} 회원님은 ${f.name} 핸드폰을 사용하고 계십니다.")

  def usePhone02(user: Option[String], phone: Option[String]): Option[String] =
    AF.map2(user, phone)((u, f) => s"$u 회원님은 $f 핸드폰을 사용하고 계십니다.")

  val user = Some(User("sslee", "sslee", 10))
  val phone = Some(MobilePhone("iPhone", "iOS"))
  val rx04 = usePhone01(user, phone)
  val rx05 = usePhone02(user map (_.name), phone map (_.name))

  println(rx04)
  println(rx05)

  val opa = new Applicative[Option] {
    override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] = fa match {
      case Some(a) => fab map (atob => atob(a))
      case None    => None
    }
    def unit[A](a: => A): Option[A] = Some(a)
    override def map2[A, B, C](ma: Option[A], mb: Option[B])(f: (A, B) => C): Option[C] = (ma, mb) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case (_, _)             => None
    }
  }

  val rs04 = opa.map2(Some(2), Some(3))(opa.productF(a => a * 2, b => b * 3)) ==
    opa.product(opa.map(Some(2))(a => a * 2), opa.map(Some(3))(b => b * 3))
  println(rs04)

}