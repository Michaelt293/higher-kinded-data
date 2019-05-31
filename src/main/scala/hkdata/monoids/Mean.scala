package hkdata.monoid

import cats.implicits._, cats._, cats.derived._

case class Mean[N](numerator: N, denominator: Int) {
  def mean(implicit numeric: Numeric[N]): Option[Float] =
    if (denominator != 0) Some(numeric.toFloat(numerator) / denominator)
    else None
}

object Mean {
  implicit def meanMonoid[N](implicit numeric: Numeric[N]) =
    new Monoid[Mean[N]] {
      def empty = Mean(numeric.zero, 0)
      def combine(m1: Mean[N], m2: Mean[N]) =
        Mean(
          numeric.plus(m1.numerator, m2.numerator),
          m1.denominator + m2.denominator
        )
    }

  implicit val meanFunctor: Functor[Mean] = {
    import derived.auto.functor._
    derived.semi.functor
  }

  implicit val meanApplicative: Applicative[Mean] =
    new Applicative[Mean] {
      def ap[A, B](mf: Mean[A => B])(ma: Mean[A]): Mean[B] =
        (mf, ma) match {
          case (Mean(f, n1), Mean(b, n2)) => Mean(f(b), n1 + n2)
        }

      def pure[A](a: A): Mean[A] = Mean(a, 1)
    }
}
