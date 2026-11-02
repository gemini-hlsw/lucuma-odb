// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import org.tpolecat.typename.TypeName
import skunk.codec.numeric.int8

trait Igrins2DynamicTable[F[_]] extends BaseMapping[F]:

  class Igrins2DynamicTable[I](
    name:   String,
    idName: String,
    id:     skunk.Codec[I]
  )(using TypeName[I]) extends TableDef(name):
    val Id: ColumnRef           = col(idName,            id)
    val ExposureTime: ColumnRef = col("c_exposure_time", time_span)

  object Igrins2DynamicTable extends Igrins2DynamicTable(
    "t_igrins_2_dynamic",
    "c_step_id",
    step_id
  )

  object Igrins2CalibrationSetupTable extends Igrins2DynamicTable(
    "v_igrins_2_daytime_calibration_setup",
    "c_set_id",
    int8
  )
