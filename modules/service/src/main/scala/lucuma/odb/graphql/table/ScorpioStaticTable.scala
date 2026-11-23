// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.instrument
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.ScorpioCodecs.scorpio_channel_selection
import skunk.codec.numeric.int8

trait ScorpioStaticTable[F[_]] extends BaseMapping[F]:

  object ScorpioStaticTable extends TableDef("t_scorpio_static"):
    val Id: ColumnRef               = col("c_static_id",         int8)
    val ObservationId: ColumnRef    = col("c_observation_id",    observation_id)
    val Instrument: ColumnRef       = col("c_instrument",        instrument)
    val ChannelSelection: ColumnRef = col("c_channel_selection", scorpio_channel_selection)
