// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.NonEmptyList
import cats.syntax.all._
import grackle.Result
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Group
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Target
import lucuma.core.util.Timestamp
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding._

object ObservationPropertiesInput {

  trait AsterismInput {
    def targetEnvironment: Option[TargetEnvironmentInput]

    def asterism: Nullable[NonEmptyList[Target.Id]] =
      for {
        t <- Nullable.orAbsent(targetEnvironment)
        a <- t.asterism.flatMap(tids => Nullable.orAbsent(NonEmptyList.fromList(tids)))
      } yield a

  }

  final case class Create(
    subtitle:            Option[NonEmptyString],
    status:              Option[ObsStatus],
    activeStatus:        Option[ObsActiveStatus],
    visualizationTime:   Option[Timestamp],
    posAngleConstraint:  Option[PosAngleConstraintInput],
    targetEnvironment:   Option[TargetEnvironmentInput],
    constraintSet:       Option[ConstraintSetInput],
    timingWindows:       Option[List[TimingWindowInput]],
    obsAttachments:      Option[List[ObsAttachment.Id]],
    scienceRequirements: Option[ScienceRequirementsInput],
    observingMode:       Option[ObservingModeInput.Create],
    existence:           Option[Existence],
    group:               Option[Group.Id],
    groupIndex:          Option[NonNegShort],
  ) extends AsterismInput

  object Create {

    val Default: Create =
      Create(
        subtitle            = Option.empty,
        status              = ObsStatus.New.some,
        activeStatus        = ObsActiveStatus.Active.some,
        visualizationTime   = Option.empty,
        posAngleConstraint  = None,
        targetEnvironment   = None,
        constraintSet       = ConstraintSetInput.Default.some,
        timingWindows       = None,
        obsAttachments      = None,
        scienceRequirements = None,
        observingMode       = None,
        existence           = Existence.Present.some,
        group               = None,
        groupIndex          = None,
      )

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          NonEmptyStringBinding.Option("subtitle", rSubtitle),
          ObsStatusBinding.Option("status", rObsStatus),
          ObsActiveStatusBinding.Option("activeStatus", rObsActiveStatus),
          TimestampBinding.Option("visualizationTime", rVisualizationTime),
          PosAngleConstraintInput.Binding.Option("posAngleConstraint", rPosAngleConstraint),
          TargetEnvironmentInput.Binding.Option("targetEnvironment", rTargetEnvironment),
          ConstraintSetInput.Binding.Option("constraintSet", rConstraintSet),
          TimingWindowInput.Binding.List.Option("timingWindows", rTimingWindows),
          ObsAttachmentIdBinding.List.Option("obsAttachments", rObsAttachments),
          ScienceRequirementsInput.Binding.Option("scienceRequirements", rScienceRequirements),
          ObservingModeInput.Create.Binding.Option("observingMode", rObservingMode),
          ExistenceBinding.Option("existence", rExistence),
          GroupIdBinding.Option("groupId", rGroupId),
          NonNegShortBinding.Option("groupIndex", rGroupIndex),
        ) =>
          (rSubtitle,
            rObsStatus,
            rObsActiveStatus,
            rVisualizationTime,
            rPosAngleConstraint,
            rTargetEnvironment,
            rConstraintSet,
            rTimingWindows,
            rObsAttachments,
            rScienceRequirements,
            rObservingMode,
            rExistence,
            rGroupId,
            rGroupIndex,
          ).parMapN(Create.apply)
      }

  }

  final case class Edit(
    subtitle:            Nullable[NonEmptyString],
    status:              Option[ObsStatus],
    activeStatus:        Option[ObsActiveStatus],
    visualizationTime:   Nullable[Timestamp],
    posAngleConstraint:  Option[PosAngleConstraintInput],
    targetEnvironment:   Option[TargetEnvironmentInput],
    constraintSet:       Option[ConstraintSetInput],
    timingWindows:       Nullable[List[TimingWindowInput]],
    obsAttachments:      Nullable[List[ObsAttachment.Id]],
    scienceRequirements: Option[ScienceRequirementsInput],
    observingMode:       Nullable[ObservingModeInput.Edit],
    existence:           Option[Existence],
    group:               Nullable[Group.Id],
    groupIndex:          Option[NonNegShort],
  ) extends AsterismInput

  object Edit {

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          NonEmptyStringBinding.Nullable("subtitle", rSubtitle),
          ObsStatusBinding.Option("status", rObsStatus),
          ObsActiveStatusBinding.Option("activeStatus", rObsActiveStatus),
          TimestampBinding.Nullable("visualizationTime", rVisualizationTime),
          PosAngleConstraintInput.Binding.Option("posAngleConstraint", rPosAngleConstraint),
          TargetEnvironmentInput.Binding.Option("targetEnvironment", rTargetEnvironment),
          ConstraintSetInput.Binding.Option("constraintSet", rConstraintSet),
          TimingWindowInput.Binding.List.Nullable("timingWindows", rTimingWindows),
          ObsAttachmentIdBinding.List.Nullable("obsAttachments", rObsAttachments),
          ScienceRequirementsInput.Binding.Option("scienceRequirements", rScienceRequirements),
          ObservingModeInput.Edit.Binding.Nullable("observingMode", rObservingMode),
          ExistenceBinding.Option("existence", rExistence),
          GroupIdBinding.Nullable("groupId", rGroupId),
          NonNegShortBinding.NonNullable("groupIndex", rGroupIndex),
        ) =>
          (rSubtitle,
            rObsStatus,
            rObsActiveStatus,
            rVisualizationTime,
            rPosAngleConstraint,
            rTargetEnvironment,
            rConstraintSet,
            rTimingWindows,
            rObsAttachments,
            rScienceRequirements,
            rObservingMode,
            rExistence,
            rGroupId,
            rGroupIndex,
          ).parMapN(apply)
      }
  }
}
