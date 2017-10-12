package com.tw.intergalactic.core

import com.tw.intergalactic.violations.{MoreThanThreeI, RomanNumberViolation, Violation}

object RomanNumber extends Enumeration {
  type RomanNumber = Value
  val I, V, X, L, C, D, M = Value
  private val FIRST_VIOLATION_CHECK = MoreThanThreeI
  private val RomanToArabic: Map[RomanNumber, Long] = Map(
    I -> 1L,
    V -> 5L,
    X -> 10L,
    L -> 50L,
    C -> 100L,
    D -> 500L,
    M -> 1000L
  )

  def createFromString(romanNumStr: String): RomanNumber = romanNumStr.trim match {
    case "I" => I
    case "V" => V
    case "X" => X
    case "L" => L
    case "C" => C
    case "D" => D
    case "M" => M
  }

  def createFromStringList(romanNumList: List[String]): List[RomanNumber] = romanNumList.map(createFromString)

  def isValid(rn: List[RomanNumber]): Either[Violation, Boolean] = {
    def iterate(violation: RomanNumberViolation): Either[RomanNumberViolation, Boolean] = {
      if (violation.checkFor(rn)) Left(violation)
      else if (violation == violation.next) Right(true)
      else iterate(violation.next)
    }

    iterate(FIRST_VIOLATION_CHECK)
  }

  def eval(roman: List[RomanNumber]): Either[Violation, Long] = {

    def iterate(violation: RomanNumberViolation): Either[RomanNumberViolation, Long] = {
      if (violation.checkFor(roman)) Left(violation)
      else if (violation == violation.next) Right(equivalentArabicNumberFor(roman))
      else iterate(violation.next)
    }

    iterate(FIRST_VIOLATION_CHECK)
  }

  def equivalentArabicNumberFor(roman: List[RomanNumber]): Long = {
    def iterate(nums: List[RomanNumber], buffer: Long): Long = nums match {
      case List() => buffer
      case x if x.size == 1 => RomanToArabic.getOrElse(x.head, 0L) + buffer
      case x :: y :: xs if x >= y => iterate(y :: xs, RomanToArabic.getOrElse(x, 0L) + buffer)
      case x :: y :: xs if x < y => iterate(xs, (RomanToArabic.getOrElse(y, 0L) - RomanToArabic.getOrElse(x, 0L)) + buffer)
      case _ => buffer
    }

    iterate(roman, 0)
  }

}

