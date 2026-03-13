// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Query.Binding
import grackle.Query.Filter
import grackle.Query.Unique
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Igrins2OffsetMode
import lucuma.core.math.Offset
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.*
import lucuma.odb.json.offset.query.given
import lucuma.odb.sequence.igrins2.longslit.Config

trait Igrins2LongSlitMapping[F[_]]
  extends Igrins2LongSlitTable[F]
     with ExposureTimeModeMapping[F]
     with OptionalFieldMapping[F]
     with Predicates[F] { this: SkunkMapping[F] =>

  private def decodeIgrins2Offsets(s: String): Json =
    OffsetsFormat.getOption(s).map(_.asJson).getOrElse(List.empty[Offset].asJson)

  private def defaultIgrins2OffsetsJson(mode: Igrins2OffsetMode): Json =
    Config.defaultOffsetsFor(mode).map(_.asJson).asJson

  val defaultOffsetMode: FieldMapping = CursorField[Igrins2OffsetMode]("defaultOffsetMode", _ => Result(Igrins2OffsetMode.NodAlongSlit))

  val defaultSaveSVCImages: FieldMapping = CursorField[Boolean]("defaultSaveSVCImages", _ => Result(false))

  lazy val Igrins2LongSlitMapping: ObjectMapping =
    ObjectMapping(Igrins2LongSlitType)(

      SqlField("observationId", Igrins2LongSlitTable.ObservationId, key = true, hidden = true),

      SqlObject("exposureTimeMode", Join(Igrins2LongSlitTable.ObservationId, ExposureTimeModeView.ObservationId)),

      explicitOrElseDefault[Igrins2OffsetMode]("offsetMode", "explicitOffsetMode", "defaultOffsetMode"),
      SqlField("explicitOffsetMode", Igrins2LongSlitTable.OffsetMode),
      defaultOffsetMode,

      explicitOrElseDefault[Boolean]("saveSVCImages", "explicitSaveSVCImages", "defaultSaveSVCImages"),

      SqlField("explicitSaveSVCImages", Igrins2LongSlitTable.SaveSVCImages),
      defaultSaveSVCImages,

      SqlField("offsetsString", Igrins2LongSlitTable.Offsets, hidden = true),

      CursorFieldJson("offsets",
        cursor =>
          for
            s <- cursor.field("offsetsString", None).flatMap(_.as[Option[String]])
            m <- cursor.field("explicitOffsetMode", None).flatMap(_.as[Option[Igrins2OffsetMode]])
          yield s.map(decodeIgrins2Offsets)
            .getOrElse(defaultIgrins2OffsetsJson(m.getOrElse(Igrins2OffsetMode.NodAlongSlit))),
        List("offsetsString", "explicitOffsetMode")
      ),

      CursorFieldJson("explicitOffsets",
        cursor =>
          cursor
            .field("offsetsString", None)
            .flatMap(_.as[Option[String]].map(_.map(decodeIgrins2Offsets).asJson)),
        List("offsetsString")
      ),

      CursorFieldJson("defaultOffsets",
        cursor =>
          cursor
            .field("explicitOffsetMode", None)
            .flatMap(_.as[Option[Igrins2OffsetMode]])
            .map(m => defaultIgrins2OffsetsJson(m.getOrElse(Igrins2OffsetMode.NodAlongSlit))),
        List("explicitOffsetMode")
      )

    )

  lazy val Igrins2LongSlitElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (Igrins2LongSlitType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Science),
            child
          )
        )

}
