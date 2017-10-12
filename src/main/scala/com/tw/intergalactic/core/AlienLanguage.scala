package com.tw.intergalactic.core

import com.tw.intergalactic.core.RomanNumber.RomanNumber

case class AlienLanguage(alienNumberStatements: List[AlienNumberStatement]) {

  val alienToRomanNumberMap: Map[String, RomanNumber] = createAlienToRomanNumberMap()

  private def createAlienToRomanNumberMap(): Map[String, RomanNumber] = {
    alienNumberStatements.map(s => (s.name, RomanNumber.createFromString(s.romanNumeral))).toMap
  }

  def contains(token: String): Boolean = alienToRomanNumberMap.contains(token)

  def validAlienTokens(alienTokens: List[String]): Boolean = {
    def iterate(strings: List[String]): Boolean = strings match {
      case List() => true
      case x :: xs if contains(x) => iterate(xs)
      case _ => false
    }
    iterate(alienTokens)
  }

  def stringToRomanNumber(token: String): RomanNumber = alienToRomanNumberMap(token)

  def stringsToRomanNumberList(tokens: List[String]): List[RomanNumber] = tokens.map(stringToRomanNumber)

}