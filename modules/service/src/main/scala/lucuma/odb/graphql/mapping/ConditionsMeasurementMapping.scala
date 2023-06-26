// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping

import table.TargetView
import table.ProgramTable
import lucuma.odb.graphql.table.ChronConditionsEntryView

trait ConditionsMeasurementMapping[F[_]] extends ChronConditionsEntryView[F] {

  lazy val ConditionsMeasurementMapping =
    ObjectMapping(
      tpe = ConditionsMeasurementType,
      fieldMappings = List(
        SqlField("synthetic-id", ChronConditionsEntryView.Measurement.SyntheticId, key = true, hidden = true),
        SqlField("source", ChronConditionsEntryView.Measurement.Source),
        SqlObject("seeing"),
        SqlField("extinction", ChronConditionsEntryView.Measurement.ExtinctionPct),
        SqlObject("wavelength"),
        SqlObject("azimuth"),
        SqlObject("elevation"),
      )
    )

}

