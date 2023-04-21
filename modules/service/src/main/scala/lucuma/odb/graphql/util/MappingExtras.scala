// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.util

import cats.Eq
import cats.kernel.Order
import cats.syntax.all._
import edu.gemini.grackle.Cursor.Context
import edu.gemini.grackle.Mapping
import edu.gemini.grackle.Path
import edu.gemini.grackle.Type
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Encoder
import org.tpolecat.sourcepos.SourcePos

import scala.reflect.ClassTag

trait MappingExtras[F[_]] extends Mapping[F] {

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

}
