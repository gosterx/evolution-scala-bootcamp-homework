package com.evolution.bootcamp.homework.error_handling

import cats.data.ValidatedNec
import cats.syntax.all._
import com.evolution.bootcamp.homework.error_handling.ErrorHandling.Homework.PaymentCard.AllErrorsOr
import com.evolution.bootcamp.homework.error_handling.ErrorHandling.Homework.ValidationError.{ExpirationDateFormatError, ExpirationDateMonthIsNotNumeric, ExpirationDateMonthIsNotValid, ExpirationDateYearIsNotNumeric}

object ErrorHandling {

  // Homework. Place the solution under `error_handling` package in your homework repository.
  //
  // 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
  // 2. Add `ValidationError` cases (at least 5, may be more).
  // 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
  object Homework {
    sealed trait ValidationError
    object ValidationError {
      case object EmptyNameError extends ValidationError {
        override def toString: String = "Name can't be empty"
      }
      case object NumberIsNotNumeric extends ValidationError {
        override def toString: String = "Number must be a number"
      }
      case object NumberLengthError extends ValidationError {
        override def toString: String = "The number must be 16 digits"
      }
      case object ExpirationDateFormatError extends ValidationError {
        override def toString: String = "Expiration date format is invalid(must be xx/xx)"
      }
      case object ExpirationDateMonthIsNotNumeric extends ValidationError {
        override def toString: String = "Month of expiration date must be a number"
      }
      case object ExpirationDateMonthIsNotValid extends ValidationError {
        override def toString: String = "Month of expiration date must be a number from 1 to 12"
      }
      case object ExpirationDateYearIsNotNumeric extends ValidationError {
        override def toString: String = "Year of expiration date must be a number"
      }
      case object SecurityCodeIsNotNumeric extends ValidationError {
        override def toString: String = "Security code must be a number"
      }
      case object SecurityCodeLengthError extends ValidationError {
        override def toString: String = "Security code must be 3 digits"
      }
    }

    sealed abstract case class ExpirationDate private (month: Int, year: Int)
    object ExpirationDate {
      private def validateExpirationDate(expirationDate: String): AllErrorsOr[ExpirationDate] = {

        def validateExpirationDateFormatError: AllErrorsOr[String] =
          if (expirationDate.length == 5 && expirationDate(2) == '/') expirationDate.validNec
          else ExpirationDateFormatError.invalidNec

        def validateExpirationDateMonthIsNotNumeric: AllErrorsOr[Int] =
          expirationDate.slice(0, 2).toIntOption match {
            case Some(x) => x.validNec
            case None    => ExpirationDateMonthIsNotNumeric.invalidNec
          }

        def validateExpirationDateMonth(month: Int): AllErrorsOr[Int] =
          if (month > 0 && month <= 12) month.validNec
          else ExpirationDateMonthIsNotValid.invalidNec

        def validateExpirationDateYearIsNotNumeric: AllErrorsOr[Int] =
          expirationDate.slice(3, 5).toIntOption match {
            case Some(x) => x.validNec
            case None    => ExpirationDateYearIsNotNumeric.invalidNec
          }

        validateExpirationDateFormatError.andThen(_ =>
          (validateExpirationDateMonthIsNotNumeric andThen validateExpirationDateMonth, validateExpirationDateYearIsNotNumeric)
            .mapN((date, month) => new ExpirationDate(date, month) {}))
      }

      def create(expirationDate: String): AllErrorsOr[ExpirationDate] = validateExpirationDate(expirationDate)
    }

    sealed abstract case class PaymentCard private (name: String,
                           number: String,
                           expirationDate: ExpirationDate,
                           securityCode: String)
    object PaymentCard {
      type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

      import ValidationError._

      private def validateName(name: String): AllErrorsOr[String] =
        if (name.nonEmpty) name.validNec
        else EmptyNameError.invalidNec

      private def validateNumber(number: String): AllErrorsOr[String] = {
        def validateNumberIsNotNumeric: AllErrorsOr[String] = number.toIntOption match {
          case Some(x) => x.toString.validNec
          case None    => NumberIsNotNumeric.invalidNec
        }

        def validateNumberLengthError(number: String): AllErrorsOr[String] =
          if (number.length == 16) number.validNec
          else NumberLengthError.invalidNec

        validateNumberIsNotNumeric andThen validateNumberLengthError
      }

      private def validateSecurityCode(securityCode: String): AllErrorsOr[String] = {
        def validateSecurityCodeIsNotNumeric: AllErrorsOr[String] = securityCode.toIntOption match {
          case Some(x) => x.toString.validNec
          case None    => SecurityCodeIsNotNumeric.invalidNec
        }

        def validateSecurityCodeLengthError(securityCode: String): AllErrorsOr[String] =
          if (securityCode.length == 3) securityCode.validNec
          else SecurityCodeLengthError.invalidNec

        validateSecurityCodeIsNotNumeric andThen validateSecurityCodeLengthError
      }

      private def validate(
                    name: String,
                    number: String,
                    expirationDate: String,
                    securityCode: String,
                  ): AllErrorsOr[PaymentCard] =
        (validateName(name), validateNumber(number), ExpirationDate.create(expirationDate),validateSecurityCode(securityCode))
          .mapN((name, number, expirationDate, securityCode) => new PaymentCard(name, number, expirationDate, securityCode) {})

      def create(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] = validate(name, number, expirationDate, securityCode)
    }
  }
}
