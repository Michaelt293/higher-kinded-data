package hkdata

import cats._
import cats.implicits._
import shapeless.{::, HList, HNil, Generic}

trait CombineFields[A] {
  type Out

  def toCombiningF(a: A)(implicit m: Monoid[Out]): Out
}

object CombineFields {
  import monoids._

  type Aux[A, B] = CombineFields[A] { type Out = B }

  def instance[A, B](f: A => B): Aux[A, B] =
    new CombineFields[A] {
      type Out = B

      def toCombiningF(a: A)(implicit m: Monoid[B]): B = f(a)
    }

  implicit def listCombineFields[A, F[_]](
      implicit
      foldable: Foldable[F]
  ): Aux[F[A], List[A]] =
    instance(foldable.toList)

  implicit def setCombineFields[A, F[_]](
      implicit
      foldable: Foldable[F]
  ): Aux[F[A], Set[A]] =
    instance(foldable.toList(_).toSet)

  implicit def meanOptionCombineFields[A]: Aux[Option[A], MeanOption[A]] =
    instance {
      case None => MeanOption(None, 0)
      case some => MeanOption(some, 1)
    }

  implicit def meanIdCombineFields[A]: Aux[Id[A], Mean[A]] =
    instance(Mean(_, 1))

  implicit def meanOptionCombineFields[A](
      implicit numeric: Numeric[A]
  ): Aux[Option[A], Mean[A]] =
    instance {
      case None    => Mean(numeric.zero, 1)
      case Some(n) => Mean(n, 1)
    }

  implicit def maxOptionCombineFields[A]: Aux[Option[A], MaxOption[A]] =
    instance(MaxOption(_))

  implicit def minOptionCombineFields[A]: Aux[Option[A], MinOption[A]] =
    instance(MinOption(_))

  implicit def maxOptionIdCombineFields[A]: Aux[Id[A], MaxOption[A]] =
    instance(a => MaxOption(Some(a)))

  implicit def minOptionIdCombineFields[A]: Aux[Id[A], MinOption[A]] =
    instance(a => MinOption(Some(a)))

  implicit def firstCombineFields[A]: Aux[Option[A], First[Option[A]]] =
    instance(First(_))

  implicit def lastCombineFields[A]: Aux[Option[A], Last[Option[A]]] =
    instance(Last(_))

  implicit def maxIdCombineFields[A]: Aux[Id[A], Max[A]] =
    instance(Max(_))

  implicit def minIdCombineFields[A]: Aux[Id[A], Min[A]] =
    instance(Min(_))

  implicit def productIdCombineFields[A]: Aux[Id[A], Product[A]] =
    instance(Product(_))

  implicit def productOptionCombineFields[A](
      implicit numeric: Numeric[A]
  ): Aux[Option[A], Product[A]] =
    instance {
      case None    => Product(numeric.one)
      case Some(n) => Product(n)
    }

  implicit def monoidCombineFields[A, F[_]](
      implicit
      monoid: Monoid[F[A]]
  ): Aux[F[A], F[A]] =
    instance(identity)

  implicit def tuple2CombineFields[A, F[_], G[_], H[_]](
      implicit
      gAux: Aux[F[A], G[A]],
      hAux: Aux[F[A], H[A]],
      gMonoid: Monoid[G[A]],
      hMonoid: Monoid[H[A]]
  ): Aux[F[A], (G[A], H[A])] =
    instance(
      h => (gAux.toCombiningF(h), hAux.toCombiningF(h))
    )

  implicit def tuple3CombineFields[A, F[_], G[_], H[_], I[_]](
      implicit
      gAux: Aux[F[A], G[A]],
      hAux: Aux[F[A], H[A]],
      iAux: Aux[F[A], I[A]],
      gMonoid: Monoid[G[A]],
      hMonoid: Monoid[H[A]],
      iMonoid: Monoid[I[A]]
  ): Aux[F[A], (G[A], H[A], I[A])] =
    instance(
      h =>
        (
          gAux.toCombiningF(h),
          hAux.toCombiningF(h),
          iAux.toCombiningF(h)
        )
    )

  implicit def tuple4CombineFields[A, F[_], G[_], H[_], I[_], J[_]](
      implicit
      gAux: Aux[F[A], G[A]],
      hAux: Aux[F[A], H[A]],
      iAux: Aux[F[A], I[A]],
      jAux: Aux[F[A], J[A]],
      gMonoid: Monoid[G[A]],
      hMonoid: Monoid[H[A]],
      iMonoid: Monoid[I[A]],
      jMonoid: Monoid[J[A]]
  ): Aux[F[A], (G[A], H[A], I[A], J[A])] =
    instance(
      h =>
        (
          gAux.toCombiningF(h),
          hAux.toCombiningF(h),
          iAux.toCombiningF(h),
          jAux.toCombiningF(h)
        )
    )
}
