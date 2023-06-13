// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import edu.gemini.grackle.TypeRef
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning

import table.GmosDynamicTables

trait GmosDynamicMapping[F[_]] extends GmosDynamicTables[F] {

  private def dynamicMapping[G, L, U](
    typeRef: TypeRef,
    table:   GmosDynamicTable[G, L, U]
  ): ObjectMapping =
    ObjectMapping(
      tpe = typeRef,
      fieldMappings = List(
        SqlField("id",       table.Id, key = true),
        SqlObject("exposure"),

        SqlField("xBin",        table.CcdMode.Xbin,        hidden = true),
        SqlField("yBin",        table.CcdMode.Ybin,        hidden = true),
        SqlField("ampCount",    table.CcdMode.AmpCount,    hidden = true),
        SqlField("ampGain",     table.CcdMode.AmpGain,     hidden = true),
        SqlField("ampReadMode", table.CcdMode.AmpReadMode, hidden = true),

        CursorFieldJson(
          "readout",
          cursor =>
            for {
              x <- cursor.field("xBin",        None).flatMap(_.as[GmosXBinning])
              y <- cursor.field("yBin",        None).flatMap(_.as[GmosYBinning])
              c <- cursor.field("ampCount",    None).flatMap(_.as[GmosAmpCount])
              g <- cursor.field("ampGain",     None).flatMap(_.as[GmosAmpGain])
              r <- cursor.field("ampReadMode", None).flatMap(_.as[GmosAmpReadMode])
            } yield Json.obj(
              "xBin"        -> x.asJson,
              "yBin"        -> y.asJson,
              "ampCount"    -> c.asJson,
              "ampGain"     -> g.asJson,
              "ampReadMode" -> r.asJson
            ),
            List("xBin", "yBin", "ampCount", "ampGain", "ampReadMode")
        ),

        SqlField("dtax",     table.Dtax),
        SqlField("roi",      table.Roi),

        SqlObject("gratingConfig"),
        SqlField("filter",   table.Filter),
        SqlObject("fpu")
      )
    )

  // Defines a switch mapping from the step record root to prevent the mapping
  // from being picked up in the context of a generated sequence.
  private def dynamicSwitchMapping[G, L, U](
    stepRecordType: TypeRef,
    dynamicType:    TypeRef,
    table:          GmosDynamicTable[G, L, U]
  ): TypeMapping =
    SwitchMapping(
      dynamicType,
      List(
        stepRecordType / "instrumentConfig" -> dynamicMapping(dynamicType, table)
      )
    )

  lazy val GmosNorthDynamicMapping: TypeMapping =
    dynamicSwitchMapping(GmosNorthStepRecordType, GmosNorthDynamicType, GmosNorthDynamicTable)

  lazy val GmosSouthDynamicMapping: TypeMapping =
    dynamicSwitchMapping(GmosSouthStepRecordType, GmosSouthDynamicType, GmosSouthDynamicTable)

}
