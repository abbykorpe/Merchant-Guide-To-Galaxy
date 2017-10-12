package com.tw.intergalactic.core

import com.tw.intergalactic.violations.{InvalidMetalName, UnrecognisedAlienNumber, Violation}

trait Statement extends Sentence

case class AlienNumberStatement(name: String, romanNumeral: String) extends Statement

case class MetalStatement(name: String, alienValue: List[String], price: Double) extends Statement

object MetalStatement {
  def validateMetalStatement(ms: MetalStatement, alienLanguage: AlienLanguage): Either[Violation, MetalStatement] =
    validMetalStatement(ms, alienLanguage)

  private def validMetalStatement(ms: MetalStatement, alienLanguage: AlienLanguage): Either[Violation, MetalStatement] = {
    if (!tokensPresentInAlienLanguage(ms.alienValue, alienLanguage)) Left(UnrecognisedAlienNumber)
    else if (isMetalNamePresentInAlienLanguage(ms.name, alienLanguage)) Left(InvalidMetalName)
    else isValidRomanNumber(ms.alienValue, alienLanguage) match {
      case Left(s) => Left(s)
      case Right(_) => Right(ms)
    }
  }

  private def tokensPresentInAlienLanguage(tokens: List[String], alienLanguage: AlienLanguage): Boolean =
    alienLanguage.validAlienTokens(tokens)

  private def isMetalNamePresentInAlienLanguage(name: String, alienLanguage: AlienLanguage): Boolean =
    alienLanguage.contains(name)

  private def isValidRomanNumber(tokens: List[String], alienLanguage: AlienLanguage): Either[Violation, Boolean] = {
    RomanNumber.eval(alienLanguage.stringsToRomanNumberList(tokens)) match {
      case Left(x) => Left(x)
      case Right(_) => Right(true)
    }
  }
}