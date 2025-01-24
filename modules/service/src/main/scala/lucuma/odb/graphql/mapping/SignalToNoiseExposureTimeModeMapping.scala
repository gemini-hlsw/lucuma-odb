// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ObservationView


trait SignalToNoiseExposureTimeModeMapping[F[_]] extends ObservationView[F]:

  import ObservationView.ScienceRequirements.Spectroscopy.ExposureTimeMode.SignalToNoise

  lazy val SignalToNoiseExposureTimeModeMapping: ObjectMapping =
    ObjectMapping(SignalToNoiseExposureTimeModeType)(
      SqlField("id", SignalToNoise.SyntheticId, key = true, hidden = true),
      SqlField("value", SignalToNoise.Value),
      SqlObject("at")
    )