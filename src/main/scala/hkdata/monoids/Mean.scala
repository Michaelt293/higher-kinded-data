package hkdata.monoids

import cats.Monoid

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
}
