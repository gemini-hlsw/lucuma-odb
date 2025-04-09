// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.all.*
import grackle.skunk.SkunkMapping

import scala.reflect.ClassTag

trait OptionalFieldMapping[F[_]] { this: SkunkMapping[F] =>
  def explicitOrElseDefault[A: ClassTag: io.circe.Encoder](
    name:          String,
    explicitField: String,
    defaultField:  String
  ): CursorField[A] =
    CursorField[A](
      name,
      cursor => {
        (cursor.fieldAs[Option[A]](explicitField),
          cursor.fieldAs[A](defaultField)
        ).parMapN(_.getOrElse(_))
      },
      List(explicitField, defaultField)
    )
}
