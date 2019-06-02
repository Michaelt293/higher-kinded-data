package hkdata.monoids

import cats.Monoid

case class Sum[N](getSum: N) extends AnyVal

object Sum {
  implicit def sumMonoid[N](implicit numeric: Numeric[N]) =
    new Monoid[Sum[N]] {
      def empty = Sum(numeric.zero)
      def combine(s1: Sum[N], s2: Sum[N]) =
        Sum(
          numeric.plus(s1.getSum, s2.getSum)
        )
    }
}
