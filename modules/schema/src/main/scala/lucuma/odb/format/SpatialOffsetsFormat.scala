// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.format

import cats.syntax.foldable.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Offset.P
import lucuma.core.math.Offset.Q
import lucuma.core.optics.Format

import scala.util.Try

trait SpatialOffsetsFormat:

  /**
   * Format for encoding/decoding a list of offset q components as a comma-separated sstring
   */
  val OffsetsQFormat: Format[String, List[Q]] =
    Format(
      s => Try {
        if (s.trim.isEmpty) List.empty[Q]
        else s.split(",").toList.map { q =>
          Q.signedDecimalArcseconds.reverseGet(BigDecimal(q))
        }
      }.toOption,
      _.map(q => Angle.signedDecimalArcseconds.get(q.toAngle).bigDecimal.toPlainString).intercalate(",")
    )

  /**
   * Format for encoding/decoding a list of offset p components as a comma-separated sstring
   */
  val OffsetsPFormat: Format[String, List[P]] =
    Format(
      s => Try {
        if (s.trim.isEmpty) List.empty[P]
        else s.split(",").toList.map { p =>
          P.signedDecimalArcseconds.reverseGet(BigDecimal(p))
        }
      }.toOption,
      _.map(p => Angle.signedDecimalArcseconds.get(p.toAngle).bigDecimal.toPlainString).intercalate(",")
    )

  /**
   * Format for encoding/decoding spatial offsets
   * Converts between List[Offset] and comma-separated string representation (p1,q1,p2,q2,...).
   */
  val OffsetsFormat: Format[String, List[Offset]] =
    Format(
      s => Try {
        if (s.trim.isEmpty) List.empty[Offset]
        else {
          val values = s.split(",").toList.map(BigDecimal(_))
          values.grouped(2).toList.collect {
            case List(p, q) => Offset(
              Offset.P.signedDecimalArcseconds.reverseGet(p),
              Offset.Q.signedDecimalArcseconds.reverseGet(q)
            )
          }
        }
      }.toOption,
      offsets => offsets.flatMap { offset =>
        List(
          Offset.P.signedDecimalArcseconds.get(offset.p),
          Offset.Q.signedDecimalArcseconds.get(offset.q)
        )
      }.map(_.bigDecimal.toPlainString).mkString(",")
    )

object spatialOffsets extends SpatialOffsetsFormat
