package com.evolution.bootcamp.homework.testing

import Calculator._

/** Simple calculator with buttons.
 *
 * @param memory whatever is stored in the memory.
 * @param screen whatever you see on the screen.
 */
case class Calculator(memory: Int = 0, screen: Int = 0, operation: Option[Operation] = None) {

  def enter(digit: Int): Either[String, Calculator] =
    if (digit >= 0 && digit <= 9) {
      Right(this.copy(screen = screen * 10 + digit))
    } else {
      Left("digit out of range")
    }

  def plus: Calculator      = this.copy(memory = screen, screen = 0, operation = Some(Operation.Plus))
  def minus: Calculator     = this.copy(memory = screen, screen = 0, operation = Some(Operation.Minus))
  def multiply: Calculator  = this.copy(memory = screen, screen = 0, operation = Some(Operation.Multiply))
  def divide: Calculator    = this.copy(memory = screen, screen = 0, operation = Some(Operation.Divide))

  def calculate: Either[String, Calculator] = operation.getOrElse(this) match {
    case Operation.Plus     => Right(Calculator(memory = 0, screen = memory + screen))
    case Operation.Minus    => Right(Calculator(memory = 0, screen = memory - screen))
    case Operation.Multiply => Right(Calculator(memory = 0, screen = memory * screen))
    case Operation.Divide   =>
      if (screen != 0) Right(Calculator(memory = 0, screen = memory / screen))
      else Left("Division by zero")
    case _                  => Right(this)
  }
}

object Calculator {

  sealed trait Operation
  object Operation {
    object Plus extends Operation
    object Minus extends Operation
    object Multiply extends Operation
    object Divide extends Operation
  }
}