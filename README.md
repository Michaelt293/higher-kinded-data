# Higher-kinded data in Scala

I first read about higher-kinded data in a Sandy McGuire blog post aptly named "Higher-Kinded Data".
https://reasonablypolymorphic.com/blog/higher-kinded-data/

In this blog post, the example provided is written in Haskell. Here, the same approach is applied using Scala.

The basic idea explored is that there can be multiple representations of what is essentially the same data. For example, at one level of our application, we may wish to work with data where all fields are optional. Later on, we may want to work purely on records were all the fields are provided. In standard Scala, we could write out two case class definitions and provide a method to `validate` the case class with optional fields.

```scala
case class Person(name: String, age: Int)

case class OptionPerson(name: Option[String], age: Option[Int]) {
  def validate: Option[Person] =
    for {
      n <- name
      a <- age
    } yield Person(n, a)
}
```

This approach works but has several disadvantages -
* Two case classes have to be defined which have essentially the same structure.
* A `validate` method has to be defined whose implementation is rather mechanical. (Implementing the method manually is both boring and a potential source of bugs.)
* Later refactoring has the potential to introduce bugs. For example, a field may be added to `PersonOption` but not `Person`. Alternatively, renaming a field could result in field names differing between the two data representations.

All these issues may be resolved by using higher-kinded data in combination with type class derivation using Shapeless. The higher-kinded data part of this solution involves parameterising the person case class with an `F[_]` (kind `TYPE -> TYPE`). This allows data with both required and optional data fields to be represented with the same case class.

```scala
case class PersonHK[F[_]](name: F[String], age: F[Int])

object PersonHK {
  type Person = PersonHK[Id]
  type OptionPerson = PersonHK[Option]
}
```

The next part of the solution is to derive the `validate` method generically. This can be done by first defining a `Validate` type class.

```scala
trait Validate[A] {
  type Validated

  def validate(unValidated: A): Option[Validated]
}
```

Using shapeless’s `HList` data structure and `Generic` type class, it is then possible to derive type class instances generically (see the code for more detail). Finally, an implicit class can be defined to provide an extension method. As an added bonus, this also improves type inference.

```scala
implicit class ValidateOps[HK[_[_]]](unValidated: HK[Option]) {
  def validate(implicit aux: Aux[HK[Option], HK[Id]]): Option[HK[Id]] =
    aux.validate(unValidated)
}
```

With that done, here's an example an of this code in action!

```scala
PersonHK[Option](Some("Michael"), Some(65)).validate == Some(PersonHK[Id]("Michael", 65))
```