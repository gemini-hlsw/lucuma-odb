// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.DatasetReferenceView
import table.ObservationReferenceView

trait DatasetReferenceMapping[F[_]]
  extends DatasetReferenceView[F]
     with ObservationReferenceView[F]:

  lazy val DatasetReferenceMapping: ObjectMapping =
    ObjectMapping(DatasetReferenceType)(
      SqlField("id",            DatasetReferenceView.Id, key = true, hidden = true),
      SqlObject("observation",  Join(DatasetReferenceView.ObservationId, ObservationReferenceView.Id)),
      SqlField("stepIndex",     DatasetReferenceView.StepIndex),
      SqlField("exposureIndex", DatasetReferenceView.ExposureIndex),
      SqlField("label",         DatasetReferenceView.DatasetReference),

      // Used for WHERE clause matching
      SqlField("labelString", DatasetReferenceView.DatasetReferenceString, hidden = true)
    )