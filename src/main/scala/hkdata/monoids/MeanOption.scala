package hkdata.monoid

import cats.implicits._, cats._, cats.derived._

case class MeanOption[N](numerator: Option[N], denominator: Int) {
  def mean(implicit numeric: Numeric[N]): Option[Float] =
    if (denominator != 0) numerator.map(numeric.toFloat(_) / denominator)
    else None
}

object MeanOption {
  implicit def meanOptionMonoid[N](implicit numeric: Numeric[N]) =
    new Monoid[MeanOption[N]] {
      def empty = MeanOption(None, 0)
      def combine(m1: MeanOption[N], m2: MeanOption[N]) =
        MeanOption(
          (m1.numerator, m2.numerator).mapN(numeric.plus),
          m1.denominator + m2.denominator
        )
    }
}
