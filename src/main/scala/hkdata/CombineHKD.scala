package hkdata

import cats._
import cats.implicits._
import shapeless.{::, HList, HNil, Generic}

trait CombineHKD[A] {
  type Out

  def toCombiningF(a: A)(implicit m: Monoid[Out]): Out
  def combineHKD(a1: A, a2: A)(implicit m: Monoid[Out]): Out =
    m.combine(toCombiningF(a1), toCombiningF(a2))
}

object CombineHKD {
  import monoid.MeanOption
  import monoid.MaxOption
  import monoid.MinOption

  type Aux[A, B] = CombineHKD[A] { type Out = B }

  def instance[A, B](f: A => B): Aux[A, B] =
    new CombineHKD[A] {
      type Out = B

      def toCombiningF(a: A)(implicit m: Monoid[B]): B = f(a)
    }

  implicit def listCombineHKD[A, F[_]](
      implicit
      foldable: Foldable[F]
  ): Aux[F[A], List[A]] =
    instance(foldable.toList)

  implicit def setCombineHKD[A, F[_]](
      implicit
      foldable: Foldable[F]
  ): Aux[F[A], Set[A]] =
    instance(foldable.toList(_).toSet)

  implicit def idApplicativeCombineHKD[A, F[_]](
      implicit
      applicative: Applicative[F],
      monoid: Monoid[F[A]]
  ): Aux[Id[A], F[A]] =
    instance(applicative.pure)

  implicit def meanOptionCombineHKD[A, F[_]]: Aux[Option[A], MeanOption[A]] =
    instance {
      case None => MeanOption(None, 0)
      case some => MeanOption(some, 1)
    }

  implicit def maxOptionCombineHKD[A, F[_]]: Aux[Option[A], MaxOption[A]] =
    instance(MaxOption(_))

  implicit def minOptionCombineHKD[A, F[_]]: Aux[Option[A], MinOption[A]] =
    instance(MinOption(_))

  implicit def applicativeCombineHKD[A, F[_], G[_]](
      implicit
      applicative: Applicative[G],
      monoid: Monoid[G[F[A]]]
  ): Aux[F[A], G[F[A]]] =
    instance(applicative.pure)

  implicit def monoidCombineHKD[A, F[_]](
      implicit
      monoid: Monoid[F[A]]
  ): Aux[F[A], F[A]] =
    instance(identity)

  implicit def tuple2CombineHKD[A, F[_], G[_], H[_]](
      implicit
      gAux: Aux[F[A], G[A]],
      hAux: Aux[F[A], H[A]],
      gMonoid: Monoid[G[A]],
      hMonoid: Monoid[H[A]]
  ): Aux[F[A], (G[A], H[A])] =
    instance(
      h => (gAux.toCombiningF(h), hAux.toCombiningF(h))
    )

  implicit def tuple3CombineHKD[A, F[_], G[_], H[_], I[_]](
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

  implicit def tuple4CombineHKD[A, F[_], G[_], H[_], I[_], J[_]](
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

  implicit val hnilCombineHKD: Aux[HNil, HNil] = instance(identity)

  implicit def hconsCombineHKD[A, F[_], G[_], T <: HList, T1 <: HList](
      implicit
      tAux: Aux[T, T1],
      hAux: Aux[F[A], G[A]],
      t1Monoid: Monoid[T1],
      gMonoid: Monoid[G[A]]
  ): Aux[F[A] :: T, G[A] :: T1] =
    instance {
      case h :: t =>
        hAux.toCombiningF(h) :: tAux.toCombiningF(t)
    }

  implicit def genericCombineHKD[A, ARepr <: HList, B, BRepr <: HList](
      implicit
      genA: Generic.Aux[A, ARepr],
      genB: Generic.Aux[B, BRepr],
      aux: Aux[ARepr, BRepr],
      aReprMonoid: Monoid[ARepr],
      bReprMonoid: Monoid[BRepr]
  ): Aux[A, B] =
    instance(
      a1 => genB.from(aux.toCombiningF(genA.to(a1)))
    )

  implicit val hnilMonoid =
    new Monoid[HNil] {
      def empty = HNil
      def combine(m1: HNil, m2: HNil) =
        HNil
    }

  implicit def hconsMonoid[A, F[_], T <: HList](
      implicit
      faMonoid: Monoid[F[A]],
      tMonoid: Monoid[T]
  ): Monoid[F[A] :: T] =
    new Monoid[F[A] :: T] {
      def empty = faMonoid.empty :: tMonoid.empty
      def combine(m1: F[A] :: T, m2: F[A] :: T) = (m1, m2) match {
        case (h1 :: t1, h2 :: t2) =>
          faMonoid.combine(h1, h2) :: tMonoid.combine(t1, t2)
      }
    }

  implicit def genericMonoid[A, ARepr <: HList](
      implicit
      genA: Generic.Aux[A, ARepr],
      aReprMonoid: Monoid[ARepr]
  ): Monoid[A] =
    new Monoid[A] {
      def empty = genA.from(aReprMonoid.empty)
      def combine(a1: A, a2: A) =
        genA.from(aReprMonoid.combine(genA.to(a1), genA.to(a2)))
    }

  implicit class CombineHKDOps[HKD[_[_]], F[_]](hkd1: HKD[F]) {
    def combineHKD[G[_]](
        hkd2: HKD[F]
    )(implicit aux: Aux[HKD[F], HKD[G]], monoid: Monoid[HKD[G]]): HKD[G] =
      aux.combineHKD(hkd1, hkd2)
  }

  implicit class FunctorCombineHKDOps[F[_], HKD[_[_]], G[_]](data: F[HKD[G]]) {
    def concatHKD[H[_]](
        implicit
        foldable: Foldable[F],
        aux: Aux[HKD[G], HKD[H]],
        monoid: Monoid[HKD[H]]
    ): HKD[H] =
      foldable.foldMap(data)(aux.toCombiningF)
  }
}
