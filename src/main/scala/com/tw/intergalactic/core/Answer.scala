package com.tw.intergalactic.core

trait Answer extends Sentence

case class AlienNumberAnswer(alienValue: List[String], price: Long) extends Answer

case class MetalAnswer(alienValue: List[String], name: String, price: Long) extends Answer