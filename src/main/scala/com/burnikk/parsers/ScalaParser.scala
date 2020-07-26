package com.burnikk.parsers

// https://dzone.com/articles/introduction-to-scala-parser-and-combinators
// https://dzone.com/articles/getting-started-with-scala-parser-combinators

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

class ScalaParser extends RegexParsers {
  def expression: Parser[Int] = (number ^^ {
    _.toInt
  }) ~ opt(symbol ~ expression) ^^ {
    case a ~ None => validateAndExtractValue(a)
    case a ~ Some("*" ~ b) => validateAndExtractValue(a) * validateAndExtractValue(b)
    case a ~ Some("/" ~ b) => validateAndExtractValue(a) / validateAndExtractValue(b)
    case a ~ Some("+" ~ b) => validateAndExtractValue(a) + validateAndExtractValue(b)
    case a ~ Some("-" ~ b) => validateAndExtractValue(a) - validateAndExtractValue(b)
  }

  def symbol: Parser[Any] = "+" | "-" | "*" | "/"

  def number: Parser[Int] =
    """(0|[1-9]\d*)""".r ^^ {
      _.toInt
    }

  def validateAndExtractValue(Operand: Any): Int = {
    val firstValue: Try[Int] = Try(Operand.toString.toInt)
    firstValue match {
      case util.Success(value) => value
      case util.Failure(exception) => throw new Exception("can not convert values to integer")
    }
  }

}

object TestSimpleParser extends ScalaParser {
  def main(args: Array[String]): Unit = {

    val parser = new ScalaParser

    val result = parser.parseAll(parser.expression, "9*8+21/3")
    println(result.get)

    parse(expression, "6/3+1") match {
      case Success(result, _) => println(result)
      case Failure(msg, _) => println("FAILURE: " + msg)
      case Error(msg, _) => println("ERROR: " + msg)
    }
  }
}