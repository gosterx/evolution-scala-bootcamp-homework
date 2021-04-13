package com.evolution.bootcamp.homework.json

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Test.Parameters

class JsonSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  import Json._

  implicit val params: Parameters = Parameters.default.withMinSuccessfulTests(1000)

  def jsonGen: Gen[Json] = Gen.choose(0, 10).flatMap(jsonGen)

  // Generator selection
  def jsonGen(number: Int): Gen[Json] = number match {
    case 0 => simpleJsonGen
    case n => Gen.oneOf(objectJsonGen(n - 1), arrayJsonGen(n - 1))
  }

  def tupleGen[V](key: Gen[String],value: Gen[V]): Gen[(String, V)] = for {
    k <- key
    v <- value
  } yield (k, v)

  def objectJsonGen[V](count: Int): Gen[JObject] =
    Gen.mapOfN(count, tupleGen(Gen.alphaStr, jsonGen(count))).map(JObject)

  def arrayJsonGen(count: Int): Gen[Json] = Gen.containerOfN[Vector, Json](count, jsonGen(count)).map(JArray)

  def simpleJsonGen: Gen[Json] =
    Gen.oneOf(Gen.const(JNull), jsonBooleanGen, jsonNumberGen, jsonStringGen)

  def jsonBooleanGen: Gen[JBoolean] = Gen.oneOf(true, false).map(JBoolean)
  def jsonNumberGen: Gen[JNumber] = Gen.double.map(JNumber)
  def jsonStringGen: Gen[JString] = Gen.alphaStr.map(JString)

  "parse" should "invert print" in {
    forAll(jsonGen) { json =>
      parse(print(json)).contains(json)
    }
  }

}
