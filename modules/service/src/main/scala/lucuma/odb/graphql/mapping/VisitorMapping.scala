// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.VisitorTable

trait VisitorMapping[F[_]] extends VisitorTable[F]:
  this: SkunkMapping[F] =>
  
  lazy val VisitorMapping =
    ObjectMapping(VisitorType)(
      SqlField("observationId", VisitorTable.ObservationId, key = true, hidden = true),
      SqlField("mode", VisitorTable.ObservingModeType),
      SqlObject("centralWavelength"),
      SqlObject("guideStarMinSep"),
    )