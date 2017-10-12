package com.tw.intergalactic.core

import com.tw.intergalactic.Questions

trait Sentence

object Sentence {
  def groupBySentences(sentences: List[Sentence]): Map[String, List[Sentence]] = {
    sentences.groupBy(_.toString.split("\\(").head)
  }

  def sentencesToQuestions(sentences: List[Sentence]): Questions = {
    def go(remaining: List[Sentence], buff: Questions): Questions = remaining match {
      case List() => buff
      case h :: t => h match {
        case _: AlienNumberQuestion => go(t, buff ++ List(h))
        case _: MetalQuestion => go(t, buff ++ List(h))
        case _: UnrecognisedQuestion => go(t, buff ++ List(h))
        case _ => go(t, buff)
      }
    }

    go(sentences, List[Sentence]())
  }
}