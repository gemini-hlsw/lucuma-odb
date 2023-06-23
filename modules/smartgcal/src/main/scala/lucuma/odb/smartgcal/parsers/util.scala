// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.parse.Rfc5234.char
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import lucuma.core.parser.MiscParsers.comma
import lucuma.core.util.Enumerated

import java.util.regex.PatternSyntaxException
import scala.collection.immutable.ListMap
import scala.util.control.Exception.catching
import scala.util.matching.Regex

trait UtilityParsers {

  private val regex: Regex = "^\\$(?<pattern>.+)".r
  private val splat: Regex = "(?<prefix>.*)\\*$".r

  /**
   * Parser for legacy smart GCAL definition keys. The keys provide patterns
   * that match multiple values by regex, a terminating wildcard splat *, or
   * an exact match.
   *
   * @return Parser that matches the key, producing a List[A]
   *
   * @see [[https://github.com/gemini-hlsw/ocs/blob/develop/bundle/edu.gemini.pot/src/main/java/edu/gemini/spModel/gemini/calunit/smartgcal/maps/BaseCalibrationMap.java#L29-L72]]
   */
  def manyOf[A](kv: (String, A)*): Parser[NonEmptyList[A]] = {

    val m: Map[String, A] = ListMap.from(kv)

    def formatNoMatch(prefix: String): String =
      s"$prefix, must match one of the strings: ${m.keys.map(s => s"'$s'").mkString("{", ", ", "}")}"

    def matchingRegex(pat: String): Either[String, NonEmptyList[A]] =
      catching(classOf[PatternSyntaxException])
        .either(new Regex(pat))
        .leftMap(_ => s"Invalid regex pattern $pat")
        .flatMap { r =>
          m.toList.collect { case (key, value) if r.matches(key) => value } match {
            case Nil     => formatNoMatch(s"Pattern '$pat' matched nothing").asLeft
            case a :: as => NonEmptyList(a, as).asRight
          }
        }

    def matching(key: String): Either[String, NonEmptyList[A]] =
      key match {
        case regex(pat)    => matchingRegex(pat)
        case splat(prefix) => matchingRegex(raw"^${Regex.quote(prefix)}.*$$")
        case _             => m.get(key).toRight(formatNoMatch(s"Key '$key' not found")).map(NonEmptyList.one)
      }

    Parser.repUntil(char, comma).string.flatMap { s =>
      matching(s.trim).fold(
        msg => Parser.failWith[NonEmptyList[A]](msg),
        as  => Parser.pure(as)
      )
    }
  }

  def manyOfOption[A](noneValue: String, kv: (String, A)*): Parser[NonEmptyList[Option[A]]] =
    manyOf(optionKv(noneValue, kv)*)

  def manyOfEnumerated[A](using e: Enumerated[A]): Parser[NonEmptyList[A]] =
    manyOf(enumeratedKv[A]*)

  def manyOfOptionEnumerated[A](noneValue: String)(using e: Enumerated[A]): Parser[NonEmptyList[Option[A]]] =
    manyOf(optionKv(noneValue, enumeratedKv[A])*)

  def oneOf[A](kv: (String, A)*): Parser[A] =
    Parser.fromStringMap(ListMap.from(kv))

  def oneOfOption[A](noneValue: String, kv: (String, A)*): Parser[Option[A]] =
    oneOf(optionKv(noneValue, kv)*)

  def oneOfEnumerated[A](using e: Enumerated[A]): Parser[A] =
    oneOf(enumeratedKv[A]*)

  def oneOfOptionEnumerated[A](noneValue: String)(using e: Enumerated[A]): Parser[Option[A]] =
    oneOf(optionKv(noneValue, enumeratedKv[A])*)

  private def enumeratedKv[A](using e: Enumerated[A]): Seq[(String, A)] =
    e.all.fproductLeft(a => e.tag(a))

  private def optionKv[A](noneValue: String, kv: Seq[(String, A)]): Seq[(String, Option[A])] =
    (noneValue, none[A]) +: kv.map(_.map(_.some))

}

object util extends UtilityParsers
