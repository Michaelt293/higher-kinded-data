package hkdata.monoids

import cats.Monoid

case class Product[N](getProduct: N) extends AnyVal

object Product {
  implicit def productMonoid[N](implicit numeric: Numeric[N]) =
    new Monoid[Product[N]] {
      def empty = Product(numeric.one)
      def combine(p1: Product[N], p2: Product[N]) =
        Product(
          numeric.times(p1.getProduct, p2.getProduct)
        )
    }
}
