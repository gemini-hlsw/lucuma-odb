// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.apply.*
import edu.gemini.grackle.TypeRef
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.GmosCustomSlitWidth

import table.GmosDynamicTables

trait GmosFpuMapping[F[_]] extends GmosDynamicTables[F] {

  private def fpuMapping[G, L, U](
    typeRef: TypeRef,
    table:   GmosDynamicTable[G, L, U]
  ): ObjectMapping =
    ObjectMapping(
      tpe = typeRef,
      fieldMappings = List(
        SqlField("id",        table.Id, key = true, hidden = true),

        SqlField("filename",  table.Fpu.CustomMaskFilename,  hidden = true),
        SqlField("slitWidth", table.Fpu.CustomMaskSlitWidth, hidden = true),

        CursorFieldJson(
          "customMask",
          cursor =>
            for {
              f <- cursor.field("filename",  None).flatMap(_.as[Option[String]])
              w <- cursor.field("slitWidth", None).flatMap(_.as[Option[GmosCustomSlitWidth]])
            } yield (f, w).mapN { (fʹ, wʹ) => Json.obj(
              "filename"  -> fʹ.asJson,
              "slitWidth" -> wʹ.asJson
            )}.asJson,
          List("filename", "slitWidth")
        ),

        SqlField("builtin", table.Fpu.Builtin)
      )
    )

  // Defines a switch mapping from the step record root to prevent the mapping
  // from being picked up in the context of a generated sequence.
  private def fpuSwitchMapping[G, L, U](
    stepRecordType: TypeRef,
    fpuType:        TypeRef,
    table:          GmosDynamicTable[G, L, U]
  ): TypeMapping =
    SwitchMapping(
      fpuType,
      List(
        stepRecordType / "instrumentConfig" / "fpu" -> fpuMapping(fpuType, table)
      )
    )

  lazy val GmosNorthFpuMapping: TypeMapping =
    fpuSwitchMapping(GmosNorthStepRecordType, GmosNorthFpuType, GmosNorthDynamicTable)


  lazy val GmosSouthFpuMapping: TypeMapping =
    fpuSwitchMapping(GmosSouthStepRecordType, GmosSouthFpuType, GmosSouthDynamicTable)


}
