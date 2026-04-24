// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.schema

import cats.data.NonEmptySet
import cats.effect.Sync
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

import scala.annotation.tailrec

// SchemaStitcher can build a grackle Schema from schema files that contains import statements of the form:
// #import <type list> from <schema file name>
// <type list> is a coma separated list of the times to import, or * to import all of them
trait SchemaStitcher[F[_]]:
  def build(using SourcePos): F[Result[Schema]]

object SchemaStitcher {

  private case class SchemaStitcherImpl[F[_]: Sync](
    root:   Path,
    source: SchemaSource[F]
  ) extends SchemaStitcher[F] {

    // Process to build the schema:
    // 1. Build a schema dependency tree starting with the root schema.
    // 2. Recursively collapse the tree into a List, with dependencies first
    // 3. Merge all elements referencing the same schema. Only the left-most one is left, and its type list is replaced
    //    by the union of all the others
    // 4. Build the final schema text joining the string representation of each schema. Only the used type are put in
    //    the string.
    // 5. Build the Schema instance
    override def build(using SourcePos): F[Result[Schema]] = dependenciesTree(List.empty, root)
      .map(x => collapseToList(AllElements, x))
      .map(x => merge(x, List.empty))
      .map(_.map(_.asString).mkString("\n"))
      .map(Schema(_))

    def dependenciesTree(pathToRoot: List[Path], schemaName: Path): F[DependencyNode] =
      if pathToRoot.contains(schemaName) then
        Sync[F].raiseError(
          new Exception(s"Found circular reference when resolving schema $schemaName")
        )
      else
        source
          .resolve(schemaName)
          .compile
          .toList
          .flatMap: ll =>
            ll.map(importLineParser.parse)
              .collect:
                case Right((_, (els, path))) =>
                  dependenciesTree(pathToRoot :+ schemaName, path).map((els, _))
              .sequence
              .map(DependencyNode(schemaName, ll, _))

    def collapseToList(els: Elements, node: DependencyNode): List[SchemaNode] =
      node.dependencies.flatMap { case (s, n) => collapseToList(s, n) } :+ SchemaNode(
        node.v,
        node.src.mkString("\n"),
        els
      )

    @tailrec
    final def merge(l: List[SchemaNode], acc: List[SchemaNode]): List[SchemaNode] =
      l match
        case Nil      => acc
        case x :: Nil => acc :+ x
        case x :: xx  =>
          val (m, r) = xx.partition(_.name === x.name)
          val s      = m.fold(x)((a, b) => SchemaNode(a.name, a.src, a.elements.union(b.elements)))
          merge(r, acc :+ s)

  }

  sealed trait Elements
  case object AllElements                                extends Elements
  case class ElementList(l: NonEmptySet[NonEmptyString]) extends Elements

  extension (el: Elements)
    private def union(other: Elements): Elements = (el, other) match
      case (ElementList(l1), ElementList(l2)) => ElementList(l1 ++ l2)
      case _                                  => AllElements

  private case class DependencyNode(
    v:            Path,
    src:          List[String],
    dependencies: List[(Elements, DependencyNode)]
  )

  private case class SchemaNode(name: Path, src: String, elements: Elements) {
    def asString: String = elements match {
      case AllElements    => src
      case ElementList(l) => asString(l)
    }

    def asString(l: NonEmptySet[NonEmptyString]): String =
      Schema(src) match {
        case Result.Success(b)    =>
          resolveTypes(b.types,
                       l.toList.flatMap(x => b.types.find(_.name === x.toString)),
                       List.empty
          )
            .map(SchemaRenderer.renderTypeDefn)
            .mkString("\n")
        case Result.Warning(_, b) =>
          resolveTypes(b.types,
                       l.toList.flatMap(x => b.types.find(_.name === x.toString)),
                       List.empty
          )
            .map(SchemaRenderer.renderTypeDefn)
            .mkString("\n")
        case _                    => ""
      }

    def resolveTypes(
      types:    List[NamedType],
      newNames: List[NamedType],
      acc:      List[NamedType]
    ): List[NamedType] =
      if newNames.isEmpty then acc.distinct
      else
        val uniqueNews = newNames.distinct
        val nextVals   = uniqueNews
          .flatMap {
            case fields: TypeWithFields                => fields.fields.flatMap(_.tpe.underlying.asNamed)
            case UnionType(_, _, members, _)           => members
            case InputObjectType(_, _, inputFields, _) =>
              inputFields.flatMap(_.tpe.underlying.asNamed)
            case _                                     => List.empty
          }
          .map(_.dealias)
          .filter {
            case u: ScalarType => !u.isBuiltIn
            case v             => !acc.contains(v) && !uniqueNews.contains(v)
          }
        resolveTypes(types, nextVals, uniqueNews ++ acc)

  }

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

  // The only way to build a SchemaStitcher
  def apply[F[_]: Sync](root: Path, source: SchemaSource[F]): SchemaStitcher[F] =
    SchemaStitcherImpl(root, source)

}
