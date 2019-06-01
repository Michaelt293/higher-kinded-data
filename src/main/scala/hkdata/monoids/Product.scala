package hkdata.monoids

import cats.Monoid

case class Product[N](getProduct: N) extends AnyVal

object Product {
  implicit def productMonoid[N](implicit numeric: Numeric[N]) =
    new Monoid[Product[N]] {
      def empty = Product(numeric.one)
      def combine(m1: Product[N], m2: Product[N]) =
        Product(
          numeric.times(m1.getProduct, m2.getProduct)
        )
    }
}
