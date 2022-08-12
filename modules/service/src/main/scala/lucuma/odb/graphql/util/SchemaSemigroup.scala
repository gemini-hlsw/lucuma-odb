// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.util

import cats.Semigroup
import cats.syntax.all._
import edu.gemini.grackle.Directive
import edu.gemini.grackle.Mapping
import edu.gemini.grackle.NamedType
import edu.gemini.grackle.ObjectType
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query.Select
import edu.gemini.grackle.QueryCompiler
import edu.gemini.grackle.Result
import edu.gemini.grackle.Schema
import edu.gemini.grackle.Type
import edu.gemini.grackle.TypeRef
import org.tpolecat.sourcepos.SourcePos

/** A mixin that provides Semigroup[Schema]. */
trait SchemaSemigroup[F[_]] extends Mapping[F] {

  private implicit val SemigroupDirective: Semigroup[Directive] = (a, b) =>
    if (a.name != b.name) a
    else Directive(
      a.name,
      a.description orElse b.description,
      (a.locations ++ b.locations).distinct,
      (a.args ++ b.args).distinctBy(_.name)
    )

  private implicit val SemigroupNamedType: Semigroup[NamedType] = {
    case ((a: ObjectType), (b: ObjectType)) if sameName(a, b) =>
      ObjectType(
        a.name,
        a.description orElse b.description,
        (a.fields ++ b.fields).distinctBy(_.name),
        (a.interfaces ++ b.interfaces).distinctBy(_.name),
      )
    // todo: other named types
    case (a, _) => a
  }

  implicit val SemigroupSchema: Semigroup[Schema] = (a, b) =>
    Remapper.remap {
      new Schema {
        val pos: SourcePos = a.pos
        val types: List[NamedType] = concatAndMergeWhen(a.types, b.types)(sameName)
        val directives: List[Directive] = concatAndMergeWhen(a.directives, b.directives)(_.name == _.name)
      }
    }

  // elements in `left` merged with corresponding elements in `right`, when available, followed by unmatched elements in `right`.
  private def concatAndMergeWhen[A: Semigroup](left: List[A], right: List[A])(matches: (A, A) => Boolean): List[A] =
    left.map(la => right.find(ra => matches(la, ra)).foldLeft(la)(_ |+| _)) ++
    right.filterNot(ra => left.exists(matches(_, ra)))

  private def sameName(a: TypeMapping, b: TypeMapping): Boolean =
    sameName(a.tpe, b.tpe)

  private def sameName(a: Type, b: Type): Boolean =
    (a.asNamed, b.asNamed) match {
      case (Some(na), Some(nb)) => na.name === nb.name
      case _ => false
    }

}



