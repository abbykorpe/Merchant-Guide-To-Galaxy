package com.tw.intergalactic

import com.tw.intergalactic.core._
import com.tw.intergalactic.violations._

object Parser {

  private val FIRST_SENTENCE_EXTRACTOR = AlienNumberStatementExtractor

  def parseAlienNumberStatements(sentences: Sentences): Either[Violation, List[AlienNumberStatement]] = {
    def extractAlienNumberStatements(sentencesMap: Map[String, List[Sentence]]): List[AlienNumberStatement] = {
      sentencesMap("AlienNumberStatement").map {
        case AlienNumberStatement(x, y) => AlienNumberStatement(x, y)
      }
    }

    val parsedSentences: Either[Violation, List[Sentence]] = sequence[Sentence](sentences.map(parseOneSentence))
    parsedSentences.map(Sentence.groupBySentences).map(extractAlienNumberStatements)
  }

  private def sequence[T](eitherList: List[Either[Violation, T]]): Either[Violation, List[T]] = {
    def go(remaining: List[Either[Violation, T]], accumulator: List[T], violation: Violation): Either[Violation, List[T]] = {
      remaining match {
        case List() => Right(accumulator)
        case h :: t => h match {
          case Left(x) => Left(x)
          case Right(x) => go(t, accumulator ++ List(x), violation)
        }
      }
    }

    go(eitherList, List[T](), UnrecognisedSentence)
  }

  def parseOneSentence(sentence: String): Either[Violation, Sentence] = {
    def iterate(extractor: SentenceExtractor): Either[Violation, Sentence] = {
      if (extractor.validate(sentence)) Right(extractor.extract(sentence))
      else if (extractor == extractor.next) Left(UnrecognisedSentence)
      else iterate(extractor.next)
    }

    iterate(FIRST_SENTENCE_EXTRACTOR)
  }

  def parseMetalStatements(sentences: Sentences, alienLanguage: AlienLanguage): Either[Violation, List[MetalStatement]] = {
    def validateStatements(sentencesMap: Map[String, List[Sentence]]): Either[Violation, List[MetalStatement]] = {
      val metalStatements: List[MetalStatement] = sentencesMap("MetalStatement").map {
        case MetalStatement(x, y, z) => MetalStatement(x, y, z)
      }
      sequence[MetalStatement](metalStatements.map(MetalStatement.validateMetalStatement(_, alienLanguage)))
    }

    val parsedSentences: Either[Violation, List[Sentence]] = sequence[Sentence](sentences.map(parseOneSentence))
    parsedSentences.map(Sentence.groupBySentences).flatMap(validateStatements)
  }

  def parseAllQuestions(sentences: Sentences, alienLanguage: AlienLanguage, metalDictionary: MetalDictionary): Either[Violation, Questions] = {
    def validateQuestions(questions: Questions): Either[Violation, Questions] = {
      sequence[Sentence](questions.map(Question.validateQuestion(_, alienLanguage, metalDictionary)))
    }

    val parsedSentences: Either[Violation, List[Sentence]] = sequence[Sentence](sentences.map(parseOneSentence))
    parsedSentences.map(Sentence.sentencesToQuestions).flatMap(validateQuestions)
  }

}

