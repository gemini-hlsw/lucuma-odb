// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.schema

import cats.ApplicativeThrow
import cats.data.NonEmptySet
import cats.parse.Parser
import cats.parse.Parser.*
import cats.parse.Rfc5234.alpha
import cats.parse.Rfc5234.digit
import cats.parse.Rfc5234.wsp
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import fs2.io.file.Path
import grackle.*
import org.tpolecat.sourcepos.SourcePos
import org.typelevel.log4cats.Logger

import java.lang.System.lineSeparator

// SchemaStitcher can build a grackle Schema from schema files that contains import statements of the form:
// #import <type list> from <schema file name>
// <type list> is a coma separated list of the times to import, or * to import all of them
trait SchemaStitcher:
  def build(using SourcePos): Result[Schema]

object SchemaStitcher:
  def apply(root: Path, source: SchemaSource): SchemaStitcher =
    SchemaStitcherImpl(root, source)

  /**
   * Load a schema from the given resource path, log any results, and raise the `Result` to `F[_]`
   */
  inline def load[F[_]: ApplicativeThrow: Logger](location: String)(using SourcePos): F[Schema] =
    SchemaStitcherMacros.fromResources(location).build match
      case Result.Success(schema) =>
        Logger[F]
          .debug(s"Loaded GraphQL schema '${location}'")
          .as(schema)

      case Result.Warning(problems, schema) =>
        Logger[F]
          .warn(
            s"Loaded schema '${location}' with problems:${lineSeparator}${problems.map(_.toString).mkString_(lineSeparator).indent(4)}"
          )
          .as(schema)

      case Result.Failure(problems) =>
        ApplicativeThrow[F].raiseError(
          new Throwable(
            s"Unable to load schema '${location}':${lineSeparator}${problems.map(_.toString).mkString_(lineSeparator).indent(4)}"
          )
        )

      case Result.InternalError(error) =>
        ApplicativeThrow[F].raiseError(
          new RuntimeException(s"Unable to load schema '${location}'", error)
        )

  /**
   * SchemaStitcher without any stitching, only handles a single schema
   */
  def pure(text: String): SchemaStitcher = new SchemaStitcher:
    def build(using SourcePos): Result[Schema] = Schema(text)

  sealed trait Elements
  case object AllElements                                extends Elements
  case class ElementList(l: NonEmptySet[NonEmptyString]) extends Elements

  extension (el: Elements)
    private[schema] def union(other: Elements): Elements = (el, other) match
      case (ElementList(l1), ElementList(l2)) => ElementList(l1 ++ l2)
      case _                                  => AllElements

  private val allElementsParser: Parser[Elements]                        = char('*').as(AllElements)
  private val firstCharInTypeParser: Parser[Char]                        = alpha | charIn('_')
  private val otherCharsInTypeParser: Parser[Char]                       = firstCharInTypeParser | digit
  private val elementNameParser: Parser[NonEmptyString]                  =
    (firstCharInTypeParser ~ otherCharsInTypeParser.rep0).map { case (x, xx) =>
      NonEmptyString.unsafeFrom((x +: xx).mkString(""))
    }
  private val elementNameListParser: Parser[NonEmptySet[NonEmptyString]] =
    (elementNameParser ~ (wsp.rep0.with1 *> char(
      ','
    ) *> wsp.rep0 *> elementNameParser).backtrack.rep0)
      .map { case (x, xx) => NonEmptySet.of(x, xx*) }
  private val elementListParser: Parser[Elements]                        = elementNameListParser.map(ElementList.apply)
  private val filenameCharParser: Parser[Char]                           =
    digit | alpha | charIn('.', '_', System.getProperty("file.separator").headOption.getOrElse('/'))
  private val schemaFilenameParser: Parser[Path]                         =
    (Parser.char('\"') *> filenameCharParser.rep <* Parser.char('\"')).map(x =>
      Path(x.toList.mkString(""))
    )

  // Left public to allow testing the parser
  val importLineParser: Parser[(Elements, Path)] =
    (wsp.rep0.with1 *> char('#') *> wsp.rep0 *> string(
      "import"
    ) *> wsp.rep *> (allElementsParser | elementListParser) <* wsp.rep)
      ~ (string("from") *> wsp.rep *> schemaFilenameParser)
