// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.util

import cats.Eq
import cats.kernel.Order
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import grackle.Context
import grackle.Cursor
import grackle.Mapping
import grackle.Path
import grackle.Result
import grackle.Type
import grackle.circe.CirceMappingLike
import io.circe.Encoder
import io.circe.Json
import org.tpolecat.sourcepos.SourcePos

import scala.reflect.ClassTag

trait MappingExtras[F[_]] extends CirceMappingLike[F] {

  given Order[NonNegShort] = Order.by(_.value) // y u not exist already

  given Eq[Path] with
    def eqv(a: Path, b: Path) =
      a.rootTpe =:= b.rootTpe && a.path === b.path

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
    * `Type` and field name.
    */
    case class SwitchMapping(tpe: Type, lookup: List[(Path, ObjectMapping)])(
      using val pos: SourcePos
    ) extends TypeMapping {

      def apply(cx: Context): Option[ObjectMapping] = {

        // All paths that lead here; i.e., Query / "foo" / "bar"; FooType / "bar", BarType
        val allPaths: List[Path] = 
          (cx.rootTpe :: cx.typePath.reverse)          // All the types we traversed
            .map(_.underlying)                         // with List/Nullable removed
            .zip(cx.path.reverse.tails)                // zipped with the (remaining) path to where we are now
            .map { (t, p) => Path.from(t).prepend(p) } // turned into Path objects.

        lookup.collectFirst { case (p, m) if allPaths.exists(_ === p) => m }

      }

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

  // If the parent is a CirceCursor we just walk down and don't look to see if a defined mapping
  // for the type we're sitting on. This lets us treat json results as opaque, terminal results.
  override def mkCursorForField(parent: Cursor, fieldName: String, resultName: Option[String]): Result[Cursor] = {
    val context = parent.context
    val fieldContext = context.forFieldOrAttribute(fieldName, resultName)
    parent match {
      case CirceCursor(_, json, _, env) =>
        val f = json.asObject.flatMap(_(fieldName))
        f match {
          case None if fieldContext.tpe.isNullable => Result(CirceCursor(fieldContext, Json.Null, Some(parent), env))
          case Some(json) => Result(CirceCursor(fieldContext, json, Some(parent), env))
          case _ => Result.internalError(s"Json blob doesn't contain field '$fieldName' for type ${context.tpe}")
        }
      case _ => super.mkCursorForField(parent, fieldName, resultName)
    }
  }

}
