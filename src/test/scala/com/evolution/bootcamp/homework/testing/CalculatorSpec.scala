package com.evolution.bootcamp.homework.testing

import org.scalatest.EitherValues
import org.scalatest.freespec.AnyFreeSpec

import scala.annotation.nowarn

@nowarn
class CalculatorSpec extends AnyFreeSpec with EitherValues {

  "calculator" - {
    val calculator = Calculator()
    "enters the number correctly" in {
      assert(calculator.enter(1).right.value == Calculator(0, 1, None))
      assert(calculator.enter(7).flatMap(_.enter(7)).flatMap(_.enter(6)) == Right(Calculator(0, 776, None)))
      assert(calculator.enter(12) == Left("digit out of range"))
    }
    "does nothing" - {
      "when you just repeat pressing `=`" in {
        assert(
          calculator
            .calculate
            .flatMap(_.calculate)
            .flatMap(_.calculate)
            .flatMap(_.calculate) == Right(calculator))
      }
    }
    "adds numbers correctly" in {
      assert(
        calculator
          .enter(7)
          .map(_.plus)
          .flatMap(_.enter(5))
          .flatMap(_.calculate) == Right(Calculator(0, 12, None)))
      assert(
        calculator
          .enter(7)
          .flatMap(_.enter(6))
          .map(_.plus)
          .flatMap(_.enter(5))
          .flatMap(_.enter(5))
          .flatMap(_.calculate) == Right(Calculator(0, 131, None)))
    }
    "subtracts numbers correctly" in {
      assert(
        calculator
          .enter(7)
          .map(_.minus)
          .flatMap(_.enter(5))
          .flatMap(_.calculate) == Right(Calculator(0, 2, None)))
      assert(
        calculator
          .enter(7)
          .flatMap(_.enter(6))
          .map(_.minus)
          .flatMap(_.enter(5))
          .flatMap(_.enter(5))
          .flatMap(_.calculate) == Right(Calculator(0, 21, None)))
    }
    "multiplies numbers correctly" in {
      assert(
        calculator
          .enter(7)
          .map(_.multiply)
          .flatMap(_.enter(5))
          .flatMap(_.calculate) == Right(Calculator(0, 35, None)))
      assert(
        calculator
          .enter(7)
          .flatMap(_.enter(6))
          .map(_.multiply)
          .flatMap(_.enter(5))
          .flatMap(_.enter(5))
          .flatMap(_.calculate) == Right(Calculator(0, 4180, None)))
      assert(
        calculator
          .enter(7)
          .map(_.multiply)
          .flatMap(_.enter(0))
          .flatMap(_.calculate) == Right(Calculator(0, 0, None)))
    }
    "divides numbers correctly" in {
      assert(
        calculator
          .enter(7)
          .map(_.divide)
          .flatMap(_.enter(5))
          .flatMap(_.calculate) == Right(Calculator(0, 1, None)))
      assert(
        calculator
          .enter(7)
          .flatMap(_.enter(6))
          .flatMap(_.enter(6))
          .map(_.divide)
          .flatMap(_.enter(5))
          .flatMap(_.enter(5))
          .flatMap(_.calculate) == Right(Calculator(0, 13, None)))
      assert(
        calculator
          .enter(7)
          .map(_.divide)
          .flatMap(_.enter(0))
          .flatMap(_.calculate) == Left("Division by zero"))
    }
    "performs multiple operations correctly" in {
      assert(
        calculator
          .enter(7)
          .map(_.divide)
          .flatMap(_.enter(5))
          .flatMap(_.calculate)
          .map(_.plus)
          .flatMap(_.enter(1))
          .flatMap(_.calculate) == Right(Calculator(0, 2, None)))
      assert(
        calculator
          .enter(7)
          .map(_.plus)
          .flatMap(_.enter(5))
          .flatMap(_.calculate)
          .map(_.divide)
          .flatMap(_.enter(0))
          .flatMap(_.calculate) == Left("Division by zero"))
      assert(
        calculator
          .enter(7)
          .map(_.divide)
          .flatMap(_.enter(0))
          .flatMap(_.calculate)
          .map(_.plus)
          .flatMap(_.enter(5))
          .flatMap(_.calculate) == Left("Division by zero"))
    }

  }
}
