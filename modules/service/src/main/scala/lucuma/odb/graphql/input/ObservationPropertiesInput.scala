// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.ConstraintSet
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.data.ObsActiveStatus
import lucuma.odb.data.ObsStatus
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.util.Bindings._

final case class ObservationPropertiesInput(
  subtitle:          Nullable[NonEmptyString],
  status:            Option[ObsStatus],
  activeStatus:      Option[ObsActiveStatus],
  // visualizationTime: Option[Instant],
  // posAngleConstraint: Option[PosAngleConstraintInput],
  targetEnvironment: Option[TargetEnvironmentInput],
  constraintSet:     Option[ConstraintSetInput],
  // scienceRequirements: Option[ScienceRequirementsInput],
  // scienceMode: Option[ScienceModeInput],
  existence:         Option[Existence]
)

object ObservationPropertiesInput {

  val Default: ObservationPropertiesInput =
    ObservationPropertiesInput(
      subtitle          = Nullable.Null,
      status            = ObsStatus.New.some,
      activeStatus      = ObsActiveStatus.Active.some,
      targetEnvironment = None,
      constraintSet     = ConstraintSetInput.Default.some,
      existence         = Existence.Present.some
    )

  val CreateBinding: Matcher[ObservationPropertiesInput] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Option("subtitle", rSubtitle),
        ObsStatusBinding.Option("status", rObsStatus),
        ObsActiveStatusBinding.Option("activeStatus", rObsActiveStatus),
        ("visualizationTime", _),     // ignore for now
        ("posAngleConstraint", _),    // ignore for now
        TargetEnvironmentInput.Binding.Option("targetEnvironment", rTargetEnvironment),
        ConstraintSetInput.Binding.Option("constraintSet", rConstraintSet),
        ("scienceRequirements", _),   // ignore for now
        ("scienceMode", _),           // ignore for now
        ExistenceBinding.Option("existence", rExistence),
      ) =>
        (rSubtitle.map(Nullable.orNull), rObsStatus, rObsActiveStatus, rTargetEnvironment, rConstraintSet, rExistence).parMapN(apply)
    }

  val EditBinding: Matcher[ObservationPropertiesInput] =
    ObjectFieldsBinding.rmap {
      case List(
        NonEmptyStringBinding.Nullable("subtitle", rSubtitle),
        ObsStatusBinding.Option("status", rObsStatus),
        ObsActiveStatusBinding.Option("activeStatus", rObsActiveStatus),
        ("visualizationTime", _),     // ignore for now
        ("posAngleConstraint", _),    // ignore for now
        TargetEnvironmentInput.Binding.Option("targetEnvironment", rTargetEnvironment),     // ignore for now
        ConstraintSetInput.Binding.Option("constraintSet", rConstraintSet),
        ("scienceRequirements", _),   // ignore for now
        ("scienceMode", _),           // ignore for now
        ExistenceBinding.Option("existence", rExistence),
      ) =>
        (rSubtitle, rObsStatus, rObsActiveStatus, rTargetEnvironment, rConstraintSet, rExistence).parMapN(apply)
    }

}