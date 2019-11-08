package exercise.monad.repeat06

import basic.applicative.Applicative

trait Functor[F[_]] {
  def unit[A](a: A): F[A]
  def map[A,B](ma: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  override def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))

  override def sequence[A](xs: List[F[A]]): F[List[A]] =
    xs.foldRight(unit(List.empty[A]))((ma,mb) => map2(ma,mb)((a, xs) => a:: xs))

  override def traverse[A,B](xs: List[A])(f: A => F[B]): F[List[B]] =
    xs.foldRight[F[List[B]]](unit(List.empty[B]))((a, mb) => map2(f(a), mb)((a1, xb) => a1 :: xb ))

  def compose[A,B,C](f: A => F[B])(g: B => F[C]): A => F[C] = a =>
    flatMap(f(a))(g)

}
