package hkdata

import cats.Id
import cats.implicits._
import shapeless.{::, HList, HNil, Generic}

trait Validate[A] {
  type Validated

  def validate(unValidated: A): Option[Validated]
}

object Validate {
  type Aux[A, B] = Validate[A] { type Validated = B }

  def instance[A, B](f: A => Option[B]): Aux[A, B] =
    new Validate[A] {
      type Validated = B
      def validate(unValidated: A): Option[Validated] = f(unValidated)
    }

  implicit val hnilValidate: Aux[HNil, HNil] = instance(Some(_))

  implicit def hconsValidate[A, T <: HList, T1 <: HList](
      implicit
      aux: Aux[T, T1]
  ): Aux[Option[A] :: T, A :: T1] =
    instance {
      case h :: t =>
        (h, aux.validate(t)).mapN { case (h1, t1) => h1 :: t1 }
    }

  implicit def genericValidate[A, ARepr <: HList, B, BRepr <: HList](
      implicit
      genA: Generic.Aux[A, ARepr],
      genB: Generic.Aux[B, BRepr],
      aux: Aux[ARepr, BRepr]
  ): Aux[A, B] =
    instance(a => aux.validate(genA.to(a)).map(genB.from))

  implicit class ValidateOps[HK[_[_]]](unValidated: HK[Option]) {
    def validate(implicit aux: Aux[HK[Option], HK[Id]]): Option[HK[Id]] =
      aux.validate(unValidated)
  }
}
