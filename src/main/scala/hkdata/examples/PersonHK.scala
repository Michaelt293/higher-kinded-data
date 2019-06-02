package hkdata.examples

import cats.implicits._, cats._, cats.derived._

case class PersonHK[F[_]](name: F[String], age: F[Int])

object PersonHK {
  type Person = PersonHK[Id]
  type OptionPerson = PersonHK[Option]

  implicit def showPerson: Show[Person] = {
    import derived.auto.show._
    derived.semi.show
  }

  implicit def showOptionPerson: Show[OptionPerson] = {
    import derived.auto.show._
    derived.semi.show
  }
}
