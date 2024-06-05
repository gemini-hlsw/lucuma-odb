// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.util

import cats.Eq
import cats.kernel.Order
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import grackle.Mapping
import grackle.Path
import grackle.circe.CirceMappingLike
import io.circe.Encoder

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

}
