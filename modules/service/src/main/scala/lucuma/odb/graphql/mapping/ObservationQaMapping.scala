// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import table.ObservationQaView

trait ObservationQaMapping[F[_]] extends ObservationQaView[F]:

  lazy val ObservationQaMapping: ObjectMapping =
    ObjectMapping(ObservationQaType)(
      SqlField("id", ObservationQaView.ObservationId, key = true, hidden = true),
      SqlField("state", ObservationQaView.State),
      SqlField("computed", ObservationQaView.Computed),
      SqlField("override", ObservationQaView.Override),
      SqlObject("datasets")
    )

  lazy val DatasetQaCountsMapping: ObjectMapping =
    ObjectMapping(DatasetQaCountsType)(
      SqlField("id", ObservationQaView.ObservationId, key = true, hidden = true),
      SqlField("total", ObservationQaView.Datasets.Total),
      SqlField("pass", ObservationQaView.Datasets.Pass),
      SqlField("usable", ObservationQaView.Datasets.Usable),
      SqlField("fail", ObservationQaView.Datasets.Fail),
      SqlField("notAssessed", ObservationQaView.Datasets.NotAssessed)
    )
