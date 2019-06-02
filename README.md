# Higher-kinded data in Scala

## Data validation

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
* Later refactoring has the potential to introduce bugs. For example, a field may be added to `OptionPerson` but not `Person`. Alternatively, renaming a field could result in field names differing between the two data representations.

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

##  Type-directed aggregations using higher-kinded data

In the previous section, we developed a `validate` method which has the following type - `HK[Option] => Option[HK[Id]]`. This method is analogous to a type-level `sequence`. In this section, we will explore how higher-kinded data can be used for useful aggregations using a method analogous to `foldMap`.

For the purpose of discussion, I will use a case class `WeatherDataHK` parameterised by `F[_]]` to represent data collected from an automatic weather station.

```scala
case class WeatherDataHK[F[_]](
    temp: F[Double],
    dewPoint: F[Double],
    windSpeed: F[Int]
)
```

With this data representation, the type of `F` communicates important information -

* `WeatherDataHK[Id]` - each field contains exactly one element
* `WeatherDataHK[Option]` - each field contains zero or one element
* `WeatherDataHK[List]` - each field zero or more elements
* `WeatherDataHK[Set]` - each field zero or more unique elements

With this in mind, imagine that we had a `List` of `WeatherDataHK[Id]`, we could flatten this to a single `WeatherDataHK[List]` value where each field contains a list of values. Alternatively, if we flattened the `List` into a `WeatherDataHK[Set]`, we would collect only distinct elements.

An incredibly important method used for data aggregation is `foldMap` (from the `Foldable` type class). This method has the following type signature (from the `cats` library) -

```scala
def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B
```

With this method, we start with data of type `F[A]` and apply a function of type `A => B`  to each element and aggregate the data using an implicit `Monoid` instance. If we apply the same ideas to our higher-kinded data, we come up with the following -

* `F[A]` becomes `F[HKD[G]]`
* `A => B` becomes `G[A] => H[A]` or `G ~> H`

Therefore, instead of a function `A => B`, we need a function `G[A] => H[A]` (a natural transformation) for each field of a parameterised case class.  If we have a `Monoid` instance for `H[A]`, we then have a way to aggregate the field. If we can aggregate each field of a parameterised case class, we can perform aggregations on the higher-kinded data. We can define a type class capturing the requirements for combining higher-kinded data -

```scala
trait CombineHKD[A] { // instances for this type class can be derived using Shapeless
  type Out

  def emptyHDK: Out // parameterised case class where each field is Monoid.empty
  def toCombiningF(a: A): Out // natural transformation selecting the F used for aggregations
  def combineHKD(out1: Out, out2: Out): Out // analogous to Monoid.combine for parameterised case class
}
```

Finally, we can define an  extension method, `concatHKD`, for folding over some `Foldable` `F` containing higher-kinded data of type `HKD[G]`. Note that `concatHKD` is parameterised by `H[_]` allowing us to select the aggregation behaviour -

```scala
implicit class FoldableCombineHKDOps[F[_], HKD[_[_]], G[_]](
    data: F[HKD[G]]
) {
  def concatHKD[H[_]](
      implicit
      foldable: Foldable[F],
      hkd: CombineHKD.Aux[HKD[G], HKD[H]]
  ): HKD[H] =
    foldable.foldLeft(data, hkd.emptyHDK) { (h, g) =>
      hkd.combineHKD(h, hkd.toCombiningF(g))
    }
}
```

### Examples

The best way to get a feel for all this is to see it in action. For the sake of this example, let's say we have some weather data, `completeWeatherData`, collected during the morning -

```scala
val completeWeatherData: List[WeatherData] = List(
  WeatherDataHK[Id](21.4, 19.9, 4),
  WeatherDataHK[Id](23.1, 19.8, 11),
  WeatherDataHK[Id](26.7, 21.4, 8),
  WeatherDataHK[Id](27.2, 22.0, 8),
  WeatherDataHK[Id](27.7, 21.1, 14)
)
```

For the morning weather report, we wish to report the maximum, minimum and average values for temperature, dew point and windspeed. Since `Max`, `Min` and `Mean` data types have been defined with `Monoid` instances provided in their companion objects (in `hkdata.monoids`), we can easily calculate the required data -

```scala
scala> completeWeatherData.concatHKD[Max]
res0: hkdata.examples.WeatherDataHK[hkdata.monoids.Max] = WeatherDataHK(Max(27.7),Max(22.0),Max(14))

scala> completeWeatherData.concatHKD[Min]
res1: hkdata.examples.WeatherDataHK[hkdata.monoids.Min] = WeatherDataHK(Min(21.4),Min(19.8),Min(4))

scala> completeWeatherData.concatHKD[Mean]
res2: hkdata.examples.WeatherDataHK[hkdata.monoids.Mean] = WeatherDataHK(Mean(126.10000000000001,5),Mean(104.19999999999999,5),Mean(45,5))
```

This, however, requires three passes over our data. By defining a type alias, we can collect all the required information in a single pass over the data!

```scala
scala> type MaxMinMean[A] = (Max[A], Min[A], Mean[A])
defined type alias MaxMinMean

scala> completeWeatherData.concatHKD[MaxMinMean]
res3: hkdata.examples.WeatherDataHK[MaxMinMean] = WeatherDataHK((Max(27.7),Min(21.4),Mean(126.10000000000001,5)),(Max(22.0),Min(19.8),Mean(104.19999999999999,5)),(Max(14),Min(4),Mean(45,5)))
```

Continuing with the weather example - unfortunately, there has been some wild weather. The weather station has now failed and is not collecting data for dew point and windspeed -

```scala
val partialWeatherData: List[OptionWeatherData] = List(
  WeatherDataHK[Option](Some(28.6), Some(19.9), None),
  WeatherDataHK[Option](Some(27.1), Some(19.8), Some(66)),
  WeatherDataHK[Option](Some(26.5), Some(21.4), Some(91)),
  WeatherDataHK[Option](Some(22.2), None, Some(154)),
  WeatherDataHK[Option](Some(21.8), None, None)
)
```

For a severe weather warning, we want to include the last observations for temperature, dew point and windspeed. In addition, the technicians have asked for counts of each of the values to check whether the sensors are outputting a constant value.

```scala
scala> type LastOpt[A] = Last[Option[A]]
defined type alias LastOpt

scala> partialWeatherData.concatHKD[LastOpt]
res5: hkdata.examples.WeatherDataHK[LastOpt] = WeatherDataHK(Last(Some(21.8)),Last(Some(21.4)),Last(Some(154)))

scala> partialWeatherData.concatHKD[Counts]
res9: hkdata.examples.WeatherDataHK[hkdata.monoids.Counts] = WeatherDataHK(Counts(Map(28.6 -> 1, 22.2 -> 1, 21.8 -> 1, 27.1 -> 1, 26.5 -> 1)),Counts(Map(19.8 -> 1, 19.9 -> 1, 21.4 -> 1)),Counts(Map(91 -> 1, 66 -> 1, 154 -> 1)))
```

Many other useful aggregating monoids are defined in `hkdata.monoids` allowing a wide variety of aggregations to be performed on higher-kinded data with great ease!
