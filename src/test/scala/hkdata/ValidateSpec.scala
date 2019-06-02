package hkdata

import cats.Id
import cats.implicits._
import org.scalatest._

class ValidateSpec extends FlatSpec with Matchers {
  import hkdata.examples.PersonHK
  import hkdata.examples.PersonHK._
  import Validate._

  val testOptionPerson: OptionPerson =
    PersonHK[Option](Some("Michael"), Some(65))

  val testPerson: Person = PersonHK[Id]("Michael", 65)

  val invalidPerson1: OptionPerson =
    PersonHK[Option](None, Some(65))

  val invalidPerson2: OptionPerson =
    PersonHK[Option](Some("Michael"), None)

  val invalidPerson3: OptionPerson =
    PersonHK[Option](None, None)

  "validate" should "succeed when both fields are nonEmpty" in {
    testOptionPerson.validate shouldEqual Some(testPerson)
  }

  it should "fail when name isEmpty" in {
    assert(invalidPerson1.validate.isEmpty)
  }

  it should "fail when age isEmpty" in {
    assert(invalidPerson2.validate.isEmpty)
  }

  it should "fail when both fields are empty" in {
    assert(invalidPerson3.validate.isEmpty)
  }
}
