// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mapping

import lucuma.odb.graphql.table.ObservationView


trait ExposureTimeModeMapping[F[_]] extends ObservationView[F]:

  import ObservationView.ScienceRequirements.Spectroscopy.ExposureTimeMode

  lazy val ExposureTimeModeMapping: ObjectMapping =
    ObjectMapping(ExposureTimeModeType)(
      SqlField("id", ExposureTimeMode.SyntheticId, key = true, hidden = true),
      SqlObject("signalToNoise"),
      SqlObject("timeAndCount")
    )