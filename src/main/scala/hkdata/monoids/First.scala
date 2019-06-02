package hkdata.monoids

import cats.Monoid

case class First[A](getFirst: A) extends AnyVal

object First {
  implicit def firstMonoid[A] =
    new Monoid[First[Option[A]]] {
      val empty = First(None)
      def combine(f1: First[Option[A]], f2: First[Option[A]]) =
        First(f1.getFirst.orElse(f2.getFirst))
    }
}
