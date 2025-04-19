// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ObservationView


trait TimeAndCountExposureTimeModeMapping[F[_]] extends ObservationView[F]:

  import ObservationView.ScienceRequirements.Spectroscopy.ExposureTimeMode.TimeAndCount

  lazy val TimeAndCountExposureTimeModeMapping: ObjectMapping =
    ObjectMapping(TimeAndCountExposureTimeModeType)(
      SqlField("id", TimeAndCount.SyntheticId, key = true, hidden = true),
      SqlObject("time"),
      SqlField("count", TimeAndCount.Count),
      SqlObject("at")
    )