// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.ExchangeView

trait ExchangeMapping[F[_]] extends ExchangeView[F]:
  this: SkunkMapping[F] =>

  lazy val ExchangeMapping =
    ObjectMapping(ExchangeType)(
      SqlField("observationId", ExchangeView.ObservationId, key = true, hidden = true),
      SqlField("mode", ExchangeView.ObservingModeType),
      SqlField("keckInstrument", ExchangeView.KeckInstrument),
      SqlField("subaruInstrument", ExchangeView.SubaruInstrument),
      SqlObject("totalRequestTime"),
    )
