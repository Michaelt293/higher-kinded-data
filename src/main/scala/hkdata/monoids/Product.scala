package hkdata.monoid

import cats.implicits._, cats._, cats.derived._

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

  implicit val productFunctor: Functor[Product] = {
    import derived.auto.functor._
    derived.semi.functor
  }

  implicit val productApplicative: Applicative[Product] =
    new Applicative[Product] {
      def ap[A, B](pf: Product[A => B])(pa: Product[A]): Product[B] =
        (pf, pa) match {
          case (Product(f), Product(b)) => Product(f(b))
        }

      def pure[A](a: A): Product[A] = Product(a)
    }
}
