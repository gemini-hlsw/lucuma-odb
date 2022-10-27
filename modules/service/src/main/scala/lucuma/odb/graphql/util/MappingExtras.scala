// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.util

import cats.syntax.all._
import edu.gemini.grackle.Cursor.Context
import edu.gemini.grackle.Mapping
import edu.gemini.grackle.Type
import io.circe.Encoder
import org.tpolecat.sourcepos.SourcePos

import scala.reflect.ClassTag

trait MappingExtras[F[_]] extends Mapping[F] {

  object FieldRef {
    def apply[A](underlyingField: String): Partial[A] =
      new Partial[A](underlyingField)

    class Partial[A](underlyingField: String) {
      def as[B: Encoder](field: String, f: A => B)(implicit ev: ClassTag[A]): CursorField[B] =
        CursorField(field, _.field(underlyingField, None).flatMap(_.as[A].map(f)), List(underlyingField))
    }
  }
      
  /**
    * A dispatching `TypeMapping` that selects a different underlying mapping based on the GraphQL
    * `Type` and field name, with an optional fallback.
    */
    case class SwitchMapping(tpe: Type, lookup: List[(Type, String, ObjectMapping)], default: Option[ObjectMapping] = None)(
      using val pos: SourcePos
    ) extends TypeMapping {

      def apply(ctx: Context): Option[ObjectMapping] = {
        if ctx.tpe.underlyingObject.exists(_ =:= tpe)
        then {
          (ctx.typePath.lift(1), ctx.path.headOption).tupled.flatMap { (t, s) =>
            t.underlyingObject.flatMap { tʹ =>
              val r = lookup.collectFirst {
                case (t2, s2, om) if t2 =:= tʹ && s2 == s =>
                  om
              }
              r
            }
          }
        }
        else None
      } orElse default

    }

  // Add fallback logic here to check for `SwitchMapping`s
  override def objectMapping(context: Context): Option[ObjectMapping] =
    super.objectMapping(context) orElse {
      context.tpe.underlyingObject.flatMap { obj =>
        obj.asNamed.flatMap(typeMapping) match {
          case Some(sm: SwitchMapping) => sm.apply(context)
          case _ => None
        }
      }
    }

}
