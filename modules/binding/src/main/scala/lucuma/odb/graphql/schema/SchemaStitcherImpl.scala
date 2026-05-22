// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.schema

import cats.data.NonEmptySet
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.io.file.Path
import grackle.*
import org.tpolecat.sourcepos.SourcePos

import java.lang.System.lineSeparator
import scala.annotation.tailrec

import SchemaStitcher.*

private class SchemaStitcherImpl private[schema] (
  root:   Path,
  source: SchemaSource
) extends SchemaStitcher {

  // Process to build the schema:
  // 1. Build a schema dependency tree starting with the root schema.
  // 2. Recursively collapse the tree into a List, with dependencies first
  // 3. Merge all elements referencing the same schema. Only the left-most one is left, and its type list is replaced
  //    by the union of all the others
  // 4. Build the final schema text joining the string representation of each schema. Only the used type are put in
  //    the string.
  // 5. Build the Schema instance
  override def build(using SourcePos): Result[Schema] =
    dependenciesTree(List.empty, root)
      .map(x => collapseToList(AllElements, x))
      .map(x => merge(x, List.empty))
      .map(_.map(_.asString).mkString(lineSeparator))
      .flatMap(Schema(_))

  def dependenciesTree(pathToRoot: List[Path], schemaName: Path): Result[DependencyNode] =
    if pathToRoot.contains(schemaName) then
      Result.failure(s"Found circular reference when resolving schema $schemaName")
    else
      Result
        .catchNonFatal(source.resolve(schemaName))
        .flatMap: ll =>
          ll
            .map(importLineParser.parse)
            .traverseCollect:
              case Right((_, (els, path))) =>
                dependenciesTree(pathToRoot :+ schemaName, path).map((els, _))
            .map(DependencyNode(schemaName, ll, _))

  def collapseToList(els: Elements, node: DependencyNode): List[SchemaNode] =
    node.dependencies.flatMap { case (s, n) => collapseToList(s, n) } :+ SchemaNode(
      node.v,
      node.src.mkString(lineSeparator),
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
        resolveTypes(
          b.types,
          l.toList.flatMap(x => b.types.find(_.name === x.toString)),
          List.empty
        )
          .map(SchemaRenderer.renderTypeDefn)
          .mkString(lineSeparator)
      case Result.Warning(_, b) =>
        resolveTypes(
          b.types,
          l.toList.flatMap(x => b.types.find(_.name === x.toString)),
          List.empty
        )
          .map(SchemaRenderer.renderTypeDefn)
          .mkString(lineSeparator)
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
