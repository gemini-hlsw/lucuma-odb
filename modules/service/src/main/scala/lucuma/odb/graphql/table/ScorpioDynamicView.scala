// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import lucuma.odb.util.ScorpioCodecs.*

trait ScorpioDynamicView[F[_]] extends BaseMapping[F]:

  object ScorpioDynamicView extends TableDef("v_scorpio_dynamic"):
    val Id: ColumnRef         = col("c_step_id",    step_id)
    val Instrument: ColumnRef = col("c_instrument", instrument)
    val Fpu: ColumnRef        = col("c_fpu",        scorpio_fpu)
    val Disperser: ColumnRef  = col("c_disperser",  scorpio_disperser)

    abstract class Channel(val name: String):
      val SyntheticId: ColumnRef   = col(s"c_${name}_id",             step_id.embedded)
      val ExposureTime: ColumnRef  = col(s"c_${name}_exposure_time",  time_span.embedded)
      val ExposureCount: ColumnRef = col(s"c_${name}_exposure_count", int4_pos.embedded)
      val ReadoutMode: ColumnRef   = col(s"c_${name}_readout_mode",   scorpio_vis_readout_mode.embedded)

    object G extends Channel("g")
    object R extends Channel("r")
    object I extends Channel("i")
    object Z extends Channel("z")

    val Channels: List[Channel] = List(G, R, I, Z)
