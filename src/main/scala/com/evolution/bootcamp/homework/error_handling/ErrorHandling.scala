package com.evolution.bootcamp.homework.error_handling

import cats.data.ValidatedNec
import cats.syntax.all._

object ErrorHandling {

  // Homework. Place the solution under `error_handling` package in your homework repository.
  //
  // 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
  // 2. Add `ValidationError` cases (at least 5, may be more).
  // 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
  object Homework {
    case class ExpirationDate(month: Int, year: Int)
    case class PaymentCard(name: String,
                           number: Int,
                           expirationDate: ExpirationDate,
                           securityCode: Int)

    sealed trait ValidationError
    object ValidationError {
      final case object EmptyNameError extends ValidationError {
        override def toString: String = "Name can't be empty"
      }
      final case object NumberIsNotNumeric extends ValidationError {
        override def toString: String = "Number must be a number"
      }
      final case object NumberLengthError extends ValidationError {
        override def toString: String = "The number must be 16 digits"
      }
      final case object ExpirationDateFormatError extends ValidationError {
        override def toString: String = "Expiration date format is invalid(must be xx/xx)"
      }
      final case object ExpirationDateMonthIsNotNumeric extends ValidationError {
        override def toString: String = "Month of expiration date must be a number"
      }
      final case object ExpirationDateMonthIsNotValid extends ValidationError {
        override def toString: String = "Month of expiration date must be a number from 1 to 12"
      }
      final case object ExpirationDateYearIsNotNumeric extends ValidationError {
        override def toString: String = "Year of expiration date must be a number"
      }
      final case object SecurityCodeIsNotNumeric extends ValidationError {
        override def toString: String = "Security code must be a number"
      }
      final case object SecurityCodeLengthError extends ValidationError {
        override def toString: String = "Security code must be 3 digits"
      }
    }

    object PaymentCardValidator {

      type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

      import ValidationError._

      private def validateName(name: String): AllErrorsOr[String] =
        if (name.nonEmpty) name.validNec
        else EmptyNameError.invalidNec

      private def validateNumber(number: String): AllErrorsOr[Int] = {

        def validateNumberIsNotNumeric: AllErrorsOr[Int] = number.toIntOption match {
          case Some(x) => x.validNec
          case None    => NumberIsNotNumeric.invalidNec
        }

        def validateNumberLengthError(number: Int): AllErrorsOr[Int] =
          if (number.toString.length == 16) number.validNec
          else NumberLengthError.invalidNec

        validateNumberIsNotNumeric andThen validateNumberLengthError
      }

      // xx/xx
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
            .mapN(ExpirationDate))
      }

      private def validateSecurityCode(securityCode: String): AllErrorsOr[Int] = {

        def validateSecurityCodeIsNotNumeric: AllErrorsOr[Int] = securityCode.toIntOption match {
          case Some(x) => x.validNec
          case None    => SecurityCodeIsNotNumeric.invalidNec
        }

        def validateSecurityCodeLengthError(securityCode: Int): AllErrorsOr[Int] =
          if (securityCode.toString.length == 16) securityCode.validNec
          else SecurityCodeLengthError.invalidNec

        validateSecurityCodeIsNotNumeric andThen validateSecurityCodeLengthError
      }

      def validate(
                    name: String,
                    number: String,
                    expirationDate: String,
                    securityCode: String,
                  ): AllErrorsOr[PaymentCard] =
        (validateName(name), validateNumber(number), validateExpirationDate(expirationDate),validateSecurityCode(securityCode))
          .mapN(PaymentCard)
    }
  }

}
