// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*

trait Igrins2DynamicTable[F[_]] extends BaseMapping[F]:

  object Igrins2DynamicTable extends TableDef("t_igrins_2_dynamic"):
    val Id:           ColumnRef = col("c_step_id",       step_id)
    val ExposureTime: ColumnRef = col("c_exposure_time", time_span)
