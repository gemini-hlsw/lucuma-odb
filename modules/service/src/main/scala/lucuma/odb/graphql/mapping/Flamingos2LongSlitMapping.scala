// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Result
import grackle.skunk.SkunkMapping
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Flamingos2Decker
import lucuma.core.enums.Flamingos2ReadoutMode
import lucuma.core.math.Offset
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.table.*
import lucuma.odb.json.offset.query.given
import lucuma.odb.sequence.flamingos2.longslit.Config

trait Flamingos2LongSlitMapping[F[_]]
  extends Flamingos2LongSlitView[F] with OptionalFieldMapping[F] { this: SkunkMapping[F] =>

  private def decodeSpatialOffsets(s: String): Json =
    OffsetsFormat.getOption(s).map(_.asJson).getOrElse(List.empty[Offset].asJson)

  private val defaultSpatialOffsetsJson: Json =
    Config.DefaultSpatialOffsets.asJson

  lazy val Flamingos2LongSlitMapping: ObjectMapping =
    ObjectMapping(Flamingos2LongSlitType)(

      SqlField("observationId", Flamingos2LongSlitView.ObservationId, key = true, hidden = true),

      SqlField("disperser", Flamingos2LongSlitView.Disperser),
      SqlField("filter",    Flamingos2LongSlitView.Filter),
      SqlField("fpu",       Flamingos2LongSlitView.Fpu),

      SqlField("explicitReadMode", Flamingos2LongSlitView.ReadMode),
      SqlField("explicitReads", Flamingos2LongSlitView.Reads),

      explicitOrElseDefault[Flamingos2Decker]("decker", "explicitDecker", "defaultDecker"),
      SqlField("defaultDecker",  Flamingos2LongSlitView.DeckerDefault),
      SqlField("explicitDecker", Flamingos2LongSlitView.Decker),

      explicitOrElseDefault[Flamingos2ReadoutMode]("readoutMode", "explicitReadoutMode", "defaultReadoutMode"),
      SqlField("defaultReadoutMode",  Flamingos2LongSlitView.ReadoutModeDefault),
      SqlField("explicitReadoutMode", Flamingos2LongSlitView.ReadoutMode),

      SqlField("spatialOffsetsString", Flamingos2LongSlitView.SpatialOffsets, hidden = true),

      CursorFieldJson("spatialOffsets",
        cursor =>
          cursor
            .field("spatialOffsetsString", None)
            .flatMap(_.as[Option[String]].map(_.map(decodeSpatialOffsets)))
            .map(_.getOrElse(defaultSpatialOffsetsJson)),
        List("explicitSpatialOffsets", "defaultSpatialOffsets")
      ),

      CursorFieldJson("explicitSpatialOffsets",
        cursor =>
          cursor
            .field("spatialOffsetsString", None)
            .flatMap(_.as[Option[String]].map(_.map(decodeSpatialOffsets).asJson)),
        List("spatialOffsetsString")
      ),

      CursorFieldJson("defaultSpatialOffsets", _ => Result(defaultSpatialOffsetsJson), Nil),

      SqlField("initialDisperser", Flamingos2LongSlitView.InitialDisperser),
      SqlField("initialFilter",    Flamingos2LongSlitView.InitialFilter),
      SqlField("initialFpu",       Flamingos2LongSlitView.InitialFpu),

    )

}
