package com.tw.intergalactic.core

case class MetalDictionary(metalStatements: List[MetalStatement], alienLanguage: AlienLanguage) {
  val metals: Map[String, Metal] = createMetalList()

  private def createMetalList(): Map[String, Metal] = {
    metalStatements.map(l => {
      val quantity = RomanNumber.equivalentArabicNumberFor(alienLanguage.stringsToRomanNumberList(l.alienValue))
      (l.name, Metal(l.name, l.price / quantity, l.alienValue))
    }).toMap
  }

  def getMetal(name: String): Metal = metals(name)

  def contains(name: String): Boolean = metals.contains(name)

}

case class Metal(name: String, pricePerUnit: Double, alienNumbers: List[String])