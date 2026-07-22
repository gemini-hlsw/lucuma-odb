// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.format

import cats.syntax.foldable.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset.Q
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.optics.Format
import lucuma.core.syntax.string.*

import scala.util.Try

trait SpatialOffsetsFormat:

  /**
   * Format for encoding/decoding a list of offset q components as a comma-separated sstring
   */
  val OffsetsQFormat: Format[String, List[Q]] =
    Format(
      s => Try {
        if (s.trim.isEmpty) List.empty[Q]
        else s.split(",").toList.flatMap(parseBigDecimalOption).map(_.qArcsec)
      }.toOption,
      _.map(q => Angle.signedDecimalArcseconds.get(q.toAngle).bigDecimal.toPlainString).intercalate(",")
    )

object spatialOffsets extends SpatialOffsetsFormat
