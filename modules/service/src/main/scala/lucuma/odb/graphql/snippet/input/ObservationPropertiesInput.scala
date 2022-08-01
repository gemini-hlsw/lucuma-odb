// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package input

import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.ConstraintSet
import lucuma.odb.data.{Existence, Nullable, ObsActiveStatus, ObsStatus}
import lucuma.odb.graphql.util.Bindings._

case class ObservationPropertiesInput(
  subtitle:      Nullable[NonEmptyString],
  status:        Option[ObsStatus],
  activeStatus:  Option[ObsActiveStatus],
  // visualizationTime: Option[Instant],
  // posAngleConstraint: Option[PosAngleConstraintInput],
  // targetEnvironment: Option[TargetEnvironmentInput],
  constraintSet: Option[ConstraintSet],
  // scienceRequirements: Option[ScienceRequirementsInput],
  // scienceMode: Option[ScienceModeInput],
  existence:     Option[Existence]
)

object ObservationPropertiesInput {

  val DefaultCreate: ObservationPropertiesInput =
    ObservationPropertiesInput(
      subtitle      = Nullable.Null,
      status        = Some(ObsStatus.New),
      activeStatus  = Some(ObsActiveStatus.Active),
      existence     = Some(Existence.Present),
      constraintSet = Some(ConstraintSetInput.NominalConstraints)
    )

  val CreateBinding: Matcher[ObservationPropertiesInput] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("subtitle", rSubtitle),
        ObsStatusBinding.Option("status", rObsStatus),
        ObsActiveStatusBinding.Option("activeStatus", rObsActiveStatus),
        ("visualizationTime", _),     // ignore for now
        ("posAngleConstraint", _),    // ignore for now
        ("targetEnvironment", _),     // ignore for now
        ConstraintSetInput.CreateBinding.Option("constraintSet", rConstraintSet),
        ("scienceRequirements", _),   // ignore for now
        ("scienceMode", _),           // ignore for now
        ExistenceBinding.Option("existence", rExistence),
      ) =>
        (rSubtitle.map(Nullable.orNull), rObsStatus, rObsActiveStatus, rConstraintSet, rExistence).parMapN(apply)
    }

  val EditBinding: Matcher[ObservationPropertiesInput] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Nullable("subtitle", rSubtitle),
        ObsStatusBinding.Option("status", rObsStatus),
        ObsActiveStatusBinding.Option("activeStatus", rObsActiveStatus),
        ("visualizationTime", _),     // ignore for now
        ("posAngleConstraint", _),    // ignore for now
        ("targetEnvironment", _),     // ignore for now
        ConstraintSetInput.CreateBinding.Option("constraintSet", rConstraintSet),
        ("scienceRequirements", _),   // ignore for now
        ("scienceMode", _),           // ignore for now
        ExistenceBinding.Option("existence", rExistence),
      ) =>
        (rSubtitle, rObsStatus, rObsActiveStatus, rConstraintSet, rExistence).parMapN(apply)
    }

}