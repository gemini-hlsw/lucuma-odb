// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.all.*
import coulomb.*
import grackle.Result
import grackle.skunk.SkunkMapping
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Offset.Q
import lucuma.core.math.WavelengthDither
import lucuma.core.math.units.Nanometer
import lucuma.core.model.sequence.gmos.longslit.*
import lucuma.odb.graphql.table.*
import lucuma.odb.json.offset.query.given
import lucuma.odb.json.wavelength.query.given
import lucuma.odb.sequence.gmos.longslit.Config

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
