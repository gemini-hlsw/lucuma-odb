// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*

trait ExchangeView[F[_]] extends BaseMapping[F]:

  object ExchangeView extends TableDef("v_exchange"):
    val ObservationId      = col("c_observation_id", observation_id)
    val ObservingModeType  = col("c_observing_mode_type", exchange_observing_mode_type)
    val KeckInstrument     = col("c_keck_instrument", keck_instrument.opt)
    val SubaruInstrument   = col("c_subaru_instrument", subaru_instrument.opt)
    val TotalRequestTime   = col("c_total_request_time", time_span.embedded)
    val TotalRequestTimeId = col("c_total_request_time_id", observation_id.embedded)
