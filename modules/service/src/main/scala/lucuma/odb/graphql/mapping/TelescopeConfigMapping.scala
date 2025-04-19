// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.StepRecordView

trait TelescopeConfigMapping[F[_]] extends StepRecordView[F]:

  lazy val TelescopeConfigMapping: ObjectMapping =
    ObjectMapping(StepRecordType / "telescopeConfig")(
      SqlField("synthetic_id", StepRecordView.Id, key = true, hidden = true),
      SqlObject("offset"),
      SqlField("guiding", StepRecordView.GuideState)
    )