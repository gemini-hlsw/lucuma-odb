// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Query.Binding
import grackle.Query.Filter
import grackle.Query.Unique
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import lucuma.odb.data.ExposureTimeModeRole
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.ExposureTimeModeView
import lucuma.odb.graphql.table.ObservationView

trait ScienceRequirementsMapping[F[_]] extends ObservationView[F] with ExposureTimeModeView[F] with Predicates[F]:

  lazy val ScienceRequirementsMapping: ObjectMapping =
    ObjectMapping(ScienceRequirementsType)(
      SqlField("id", ObservationView.Id, key = true, hidden = true),
      SqlField("mode", ObservationView.ScienceRequirements.Mode),
      SqlObject(
        "exposureTimeMode",
         Join(ObservationView.Id, ExposureTimeModeView.ObservationId)
      ),
      SqlObject("spectroscopy"),
      SqlObject("imaging")
    )

  lazy val ScienceRequirementsElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (ScienceRequirementsType, "exposureTimeMode", Nil) =>
      Elab.transformChild: child =>
        Unique(
          Filter(
            Predicates.exposureTimeMode.role.eql(ExposureTimeModeRole.Requirement),
            child
          )
        )