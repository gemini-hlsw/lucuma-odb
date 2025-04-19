// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ChronConditionsEntryView

trait ConditionsMeasurementMapping[F[_]] extends ChronConditionsEntryView[F] {

  lazy val ConditionsMeasurementMapping =
    ObjectMapping(ConditionsMeasurementType)(
      SqlField("synthetic-id", ChronConditionsEntryView.Measurement.SyntheticId, key = true, hidden = true),
      SqlField("source", ChronConditionsEntryView.Measurement.Source),
      SqlObject("seeing"),
      SqlField("extinction", ChronConditionsEntryView.Measurement.Extinction),
      SqlObject("wavelength"),
      SqlObject("azimuth"),
      SqlObject("elevation"),
    )

}

