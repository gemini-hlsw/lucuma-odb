// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Attachment
import lucuma.core.model.Group
import lucuma.core.model.Target
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.*
import monocle.Focus
import monocle.Lens

object ObservationPropertiesInput {

  private val PleaseUseSchedulingField: Result[Nothing] =
    OdbError.InvalidArgument("Set timing windows via the 'scheduling' properties field.".some).asFailure

  trait AsterismInput {
    def targetEnvironment: Option[TargetEnvironmentInput]

    // Some downstream stuff wants this as a Nullable NEL
    def asterism: Nullable[NonEmptyList[Target.Id]] =
      Nullable.orAbsent(targetEnvironment).flatMap: t =>
        t match
          case TargetEnvironmentInput.Create(_, asterism, _, _, _) => Nullable.orAbsent(asterism.flatMap(NonEmptyList.fromList))
          case TargetEnvironmentInput.Edit(_, asterism, _, _, _)   => asterism.flatMap(tids => Nullable.orAbsent(NonEmptyList.fromList(tids)))

  }

  final case class Create(
    subtitle:            Option[NonEmptyString],
    scienceBand:         Option[ScienceBand],
    posAngleConstraint:  Option[PosAngleConstraintInput],
    targetEnvironment:   Option[TargetEnvironmentInput.Create],
    constraintSet:       Option[ConstraintSetInput],
    scheduling:          Option[SchedulingPropertiesInput],
    attachments:         Option[List[Attachment.Id]],
    scienceRequirements: Option[ScienceRequirementsInput],
    observingMode:       Option[ObservingModeInput.Create],
    existence:           Option[Existence],
    group:               Option[Group.Id],
    groupIndex:          Option[NonNegShort],
    observerNotes:       Option[NonEmptyString]
  ) extends AsterismInput:
    def needsStaffAccess: Boolean =
      observingMode.exists(_.needsStaffAccess)

  object Create {

    val scienceBand: Lens[Create, Option[ScienceBand]] =
      Focus[Create](_.scienceBand)

    val Default: Create =
      Create(
        subtitle            = Option.empty,
        scienceBand         = None,
        posAngleConstraint  = None,
        targetEnvironment   = None,
        constraintSet       = ConstraintSetInput.Default.some,
        scheduling          = None,
        attachments         = None,
        scienceRequirements = None,
        observingMode       = None,
        existence           = Existence.Present.some,
        group               = None,
        groupIndex          = None,
        observerNotes       = None
      )

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          NonEmptyStringBinding.Option("subtitle", rSubtitle),
          ScienceBandBinding.Option("scienceBand", rScienceBand),
          PosAngleConstraintInput.Binding.Option("posAngleConstraint", rPosAngleConstraint),
          TargetEnvironmentInput.Create.Binding.Option("targetEnvironment", rTargetEnvironment),
          ConstraintSetInput.Binding.Option("constraintSet", rConstraintSet),
          TimingWindowInput.Binding.List.Option("timingWindows", rTimingWindows),
          SchedulingPropertiesInput.Binding.Option("scheduling", rScheduling),
          AttachmentIdBinding.List.Option("attachments", rAttachments),
          ScienceRequirementsInput.Binding.Option("scienceRequirements", rScienceRequirements),
          ObservingModeInput.Create.Binding.Option("observingMode", rObservingMode),
          ExistenceBinding.Option("existence", rExistence),
          GroupIdBinding.Option("groupId", rGroupId),
          NonNegShortBinding.Option("groupIndex", rGroupIndex),
          NonEmptyStringBinding.Option("observerNotes", rObserverNotes),
        ) =>
          (rSubtitle,
            rScienceBand,
            rPosAngleConstraint,
            rTargetEnvironment,
            rConstraintSet,
            (rTimingWindows, rScheduling).parFlatMapN {
              case (Some(_), Some(_)) => PleaseUseSchedulingField
              case (Some(t), None)    => SchedulingPropertiesInput(None, Nullable.NonNull(t)).some.success
              case (None,    s)       => s.success
            },
            rAttachments,
            rScienceRequirements,
            rObservingMode,
            rExistence,
            rGroupId,
            rGroupIndex,
            rObserverNotes,
          ).parMapN(Create.apply)
      }

  }

  final case class Edit(
    subtitle:            Nullable[NonEmptyString],
    scienceBand:         Nullable[ScienceBand],
    posAngleConstraint:  Option[PosAngleConstraintInput],
    targetEnvironment:   Option[TargetEnvironmentInput.Edit],
    constraintSet:       Option[ConstraintSetInput],
    scheduling:          Nullable[SchedulingPropertiesInput],
    attachments:         Nullable[List[Attachment.Id]],
    scienceRequirements: Option[ScienceRequirementsInput],
    observingMode:       Nullable[ObservingModeInput.Edit],
    existence:           Option[Existence],
    group:               Nullable[Group.Id],
    groupIndex:          Option[NonNegShort],
    observerNotes:       Nullable[NonEmptyString],
  ) extends AsterismInput:
    def updatesAcquisition: Boolean =
      observingMode.toOption.exists(_.updatesAcquisition)

    def needsStaffAccess: Boolean =
      observingMode.toOption.exists(_.needsStaffAccess)

  object Edit {

    val Empty: Edit =
      Edit(
        subtitle =            Nullable.Absent,
        scienceBand =         Nullable.Absent,
        posAngleConstraint =  None,
        targetEnvironment =   None,
        constraintSet =       None,
        scheduling =          Nullable.Absent,
        attachments =         Nullable.Absent,
        scienceRequirements = None,
        observingMode =       Nullable.Absent,
        existence =           None,
        group =               Nullable.Absent,
        groupIndex =          None,
        observerNotes =       Nullable.Absent
      )

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          NonEmptyStringBinding.Nullable("subtitle", rSubtitle),
          ScienceBandBinding.Nullable("scienceBand", rScienceBand),
          PosAngleConstraintInput.Binding.Option("posAngleConstraint", rPosAngleConstraint),
          TargetEnvironmentInput.Edit.Binding.Option("targetEnvironment", rTargetEnvironment),
          ConstraintSetInput.Binding.Option("constraintSet", rConstraintSet),
          TimingWindowInput.Binding.List.Nullable("timingWindows", rTimingWindows),
          SchedulingPropertiesInput.Binding.Nullable("scheduling", rScheduling),
          AttachmentIdBinding.List.Nullable("attachments", rAttachments),
          ScienceRequirementsInput.Binding.Option("scienceRequirements", rScienceRequirements),
          ObservingModeInput.Edit.Binding.Nullable("observingMode", rObservingMode),
          ExistenceBinding.Option("existence", rExistence),
          GroupIdBinding.Nullable("groupId", rGroupId),
          NonNegShortBinding.NonNullable("groupIndex", rGroupIndex),
          NonEmptyStringBinding.Nullable("observerNotes", rObserverNotes)
        ) =>
          (rSubtitle,
            rScienceBand,
            rPosAngleConstraint,
            rTargetEnvironment,
            rConstraintSet,
            (rTimingWindows, rScheduling).parFlatMapN {
              case (Nullable.Null,       Nullable.Absent)     => Nullable.NonNull(SchedulingPropertiesInput(None, Nullable.Null)).success
              case (Nullable.Null,       Nullable.Null)       => Nullable.Null.success
              case (Nullable.Absent,     s)                   => s.success
              case (Nullable.NonNull(t), Nullable.Absent)     => Nullable.NonNull(SchedulingPropertiesInput(None, Nullable.NonNull(t))).success
              case _                                          => PleaseUseSchedulingField
            },
            rAttachments,
            rScienceRequirements,
            rObservingMode,
            rExistence,
            rGroupId,
            rGroupIndex,
            rObserverNotes,
          ).parMapN(apply)
      }
  }
}
