package com.evolution.bootcamp.homework.basics

import com.evolution.bootcamp.homework.basics.ControlStructures.Command._
import com.evolution.bootcamp.homework.basics.ControlStructures.ErrorMessage.{DivisionByZeroError, InvalidInputArgumentError, InvalidNumbersOfArgumentsError, InvalidOperationError}

import scala.io.Source

object ControlStructures {

  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command

  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final val DIVIDE = "divide"
  final val SUM = "sum"
  final val AVERAGE = "average"
  final val MIN = "min"
  final val MAX = "max"


  sealed trait ErrorMessage {
    def getErrorInfo: String
  }

  object ErrorMessage {
    final case class InvalidNumbersOfArgumentsError() extends ErrorMessage {
      def getErrorInfo = "invalid number of arguments"
    }
    final case class InvalidOperationError() extends ErrorMessage {
      def getErrorInfo = "invalid operation"
    }
    final case class InvalidInputArgumentError() extends ErrorMessage {
      def getErrorInfo = "invalid input arguments"
    }
    final case class DivisionByZeroError() extends ErrorMessage {
      def getErrorInfo = "division by zero"
    }
  }

  final val ERROR_PREFIX = "Error: "

  // Adjust `Result` and `ChangeMe` as you wish - you can turn Result into a `case class` and remove the `ChangeMe` if
  // you think it is the best model for your solution, or just have other `case class`-es implement `Result`
  final case class Result(command: Command, value: Double)

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    // implement this method
    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???
    x.split(' ').toList.filter(_.nonEmpty) match {
      case Nil                     => Left(InvalidInputArgumentError())
      case x :: xs if xs.nonEmpty  =>
        try {
          x match {
            case DIVIDE   =>
              if (xs.length == 2) Right(Divide(xs.head.toDouble, xs.last.toDouble))
              else Left(InvalidInputArgumentError())
            case SUM      => Right(Sum(xs.map(_.toDouble)))
            case AVERAGE  => Right(Sum(xs.map(_.toDouble)))
            case MIN      => Right(Sum(xs.map(_.toDouble)))
            case MAX      => Right(Sum(xs.map(_.toDouble)))
            case _        => Left(InvalidOperationError())
          }
        } catch {
          case _: NumberFormatException => Left(InvalidInputArgumentError())
        }
      case _ :: xs if xs.isEmpty  => Left(InvalidNumbersOfArgumentsError())
      case _                      => Left(InvalidOperationError())
    }
    // Consider how to handle extra whitespace gracefully (without errors).
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Divide(dividend, divisor) =>
        if (divisor != 0) Right(Result(Divide(dividend, divisor), dividend / divisor))
        else Left(DivisionByZeroError())
      case Sum(numbers)             => Right(Result(Sum(numbers), numbers.sum))
      case Average(numbers)         => Right(Result(Average(numbers), numbers.sum / numbers.length))
      case Min(numbers)             => Right(Result(Min(numbers), numbers.min))
      case Max(numbers)             => Right(Result(Max(numbers), numbers.max))
    }
  }

  def renderResult(x: Result): String = {
    x.command match {
      case Divide(dividend, divisor)  => f"$dividend divided by $divisor is ${x.value}"
      case Sum(numbers)               => f"the sum of ${numbers.mkString(" ")} is ${x.value}"
      case Average(numbers)           => f"the average of ${numbers.mkString(" ")} is ${x.value}"
      case Min(numbers)               => f"the min of ${numbers.mkString(" ")} is ${x.value}"
      case Max(numbers)               => f"the max of ${numbers.mkString(" ")} is ${x.value}"
    }
  }

  def process(x: String): String = {
    // the import above will enable useful operations on Either-s such as `leftMap`
    // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
    // but you can also avoid using them using pattern matching.

    // implement using a for-comprehension
    val processResult = for {
      command <- parseCommand(x)
      calculationResult <- calculate(command)
    } yield renderResult(calculationResult)

    processResult match {
      case Left(error) => ERROR_PREFIX + error.getErrorInfo
      case Right(result) => result
    }
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println

}
