package com.tw.intergalactic.core

import com.tw.intergalactic.violations.{InvalidMetalName, UnrecognisedAlienNumber, Violation}

trait Question extends Sentence

object Question {
  def validateQuestion(question: Sentence, alienLanguage: AlienLanguage, metalDictionary: MetalDictionary): Either[Violation, Sentence] =
    question match {
      case a@AlienNumberQuestion(_) => validAlienNumbers(a, alienLanguage)
      case a@MetalQuestion(_, _) => validMetalQuestion(a, alienLanguage, metalDictionary)
      case a@UnrecognisedQuestion(_) => Right(a)
      case _ => Right(question)
    }

  private def validMetalQuestion(ms: MetalQuestion, alienLanguage: AlienLanguage, metalDictionary: MetalDictionary): Either[Violation, Sentence] = {
    if (!tokensPresentInAlienLanguage(ms.alienValue, alienLanguage)) Left(UnrecognisedAlienNumber)
    else if (!namePresentInMetalDictionary(ms.name, metalDictionary)) Left(InvalidMetalName)
    else if (isMetalNamePresentInAlienLanguage(ms.name, alienLanguage)) Left(InvalidMetalName)
    else isValidRomanNumber(ms.alienValue, alienLanguage) match {
      case Left(s) => Left(s)
      case Right(_) => Right(ms)
    }
  }

  private def namePresentInMetalDictionary(metalName: String, metalDictionary: MetalDictionary): Boolean = metalDictionary.contains(metalName)

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

  private def validAlienNumbers(alienNumberQuestion: AlienNumberQuestion, alienLanguage: AlienLanguage): Either[Violation, Sentence] = {
    if (alienLanguage.validAlienTokens(alienNumberQuestion.alienValue)) Right(alienNumberQuestion)
    else Left(UnrecognisedAlienNumber)
  }
}

case class AlienNumberQuestion(alienValue: List[String]) extends Question {
  override def toString: String = s"Question($alienValue)"
}

case class MetalQuestion(alienValue: List[String], name: String) extends Question {
  override def toString: String = s"Question($alienValue, $name)"
}

case class UnrecognisedQuestion(message: String) extends Question {
  override def toString: String = s"Question($message)"
}