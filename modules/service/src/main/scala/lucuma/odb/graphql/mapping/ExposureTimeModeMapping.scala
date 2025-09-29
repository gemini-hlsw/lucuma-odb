// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.ExposureTimeModeView


trait ExposureTimeModeMapping[F[_]] extends ExposureTimeModeView[F]:

  lazy val ExposureTimeModeMapping: ObjectMapping =
    ObjectMapping(ExposureTimeModeType)(
      SqlField("id", ExposureTimeModeView.Id, key = true, hidden = true),
      SqlObject("signalToNoise"),
      SqlObject("timeAndCount")
    )