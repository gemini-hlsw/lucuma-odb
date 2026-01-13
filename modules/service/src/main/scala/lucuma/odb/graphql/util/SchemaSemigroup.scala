// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.util

import cats.Semigroup
import cats.syntax.all.*
import grackle.DirectiveDef
import grackle.EnumType
import grackle.NamedType
import grackle.ObjectType
import grackle.Schema
import grackle.Type
import org.tpolecat.sourcepos.SourcePos

/** A mixin that provides Semigroup[Schema]. */
trait SchemaSemigroup {

  private implicit val SemigroupDirectiveDef: Semigroup[DirectiveDef] = (a, b) =>
    if (a.name != b.name) a
    else DirectiveDef(
      a.name,
      a.description orElse b.description,
      (a.args ++ b.args).distinctBy(_.name),
      a.isRepeatable || b.isRepeatable,
      (a.locations ++ b.locations).distinct,
    )

  private implicit val SemigroupNamedType: Semigroup[NamedType] = {
    case ((a: ObjectType), (b: ObjectType)) if sameName(a, b) =>
      ObjectType(
        a.name,
        a.description orElse b.description,
        (a.fields ++ b.fields).distinctBy(_.name),
        (a.interfaces ++ b.interfaces).distinctBy(_.name),
        (a.directives ++ b.directives).distinctBy(_.name),
      )
    case ((a: EnumType, b: EnumType)) if sameName(a, b) =>
      EnumType(
        a.name, 
        a.description, 
        (a.enumValues ++ b.enumValues).distinctBy(_.name).filterNot(_.name == "DUMMY"), 
        (a.directives ++ b.directives).distinctBy(_.name)
      )
    // todo: other named types
    case (a, _) => a
  }

  implicit val SemigroupSchema: Semigroup[Schema] = (a, b) =>
    Remapper.remap {
      new Schema {
        val pos: SourcePos = a.pos
        val baseTypes: List[NamedType] = concatAndMergeWhen(a.types, b.types)(sameName)
        val directives: List[DirectiveDef] = concatAndMergeWhen(a.directives, b.directives)(_.name == _.name)
        def schemaExtensions =
          (a.schemaExtensions, b.schemaExtensions) match
            case (Nil, Nil) => Nil
            case _ => sys.error("Unimplemented: schema addition with schema extensions")
        def typeExtensions =
          (a.typeExtensions, b.typeExtensions) match
            case (Nil, Nil) => Nil
            case _ => sys.error("Unimplemented: schema addition with type extensions")
      }
    }

  // elements in `left` merged with corresponding elements in `right`, when available, followed by unmatched elements in `right`.
  private def concatAndMergeWhen[A: Semigroup](left: List[A], right: List[A])(matches: (A, A) => Boolean): List[A] =
    left.map(la => right.find(ra => matches(la, ra)).foldLeft(la)(_ |+| _)) ++
    right.filterNot(ra => left.exists(matches(_, ra)))

  private def sameName(a: Type, b: Type): Boolean =
    (a.asNamed, b.asNamed) match {
      case (Some(na), Some(nb)) => na.name === nb.name
      case _ => false
    }

}

object SchemaSemigroup extends SchemaSemigroup

