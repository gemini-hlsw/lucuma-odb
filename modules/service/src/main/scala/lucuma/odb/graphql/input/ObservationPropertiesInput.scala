// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Attachment
import lucuma.core.model.Group
import lucuma.core.model.Target
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*
import monocle.Focus
import monocle.Lens

object ObservationPropertiesInput {

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
    timingWindows:       Option[List[TimingWindowInput]],
    attachments:         Option[List[Attachment.Id]],
    scienceRequirements: Option[ScienceRequirementsInput],
    observingMode:       Option[ObservingModeInput.Create],
    existence:           Option[Existence],
    group:               Option[Group.Id],
    groupIndex:          Option[NonNegShort],
    observerNotes:       Option[NonEmptyString]
  ) extends AsterismInput

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
        timingWindows       = None,
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
            rTimingWindows,
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
    timingWindows:       Nullable[List[TimingWindowInput]],
    attachments:         Nullable[List[Attachment.Id]],
    scienceRequirements: Option[ScienceRequirementsInput],
    observingMode:       Nullable[ObservingModeInput.Edit],
    existence:           Option[Existence],
    group:               Nullable[Group.Id],
    groupIndex:          Option[NonNegShort],
    observerNotes:       Nullable[NonEmptyString],
  ) extends AsterismInput

  object Edit {

    val Empty: Edit =
      Edit(
        subtitle =            Nullable.Absent,
        scienceBand =         Nullable.Absent,
        posAngleConstraint =  None,
        targetEnvironment =   None,
        constraintSet =       None,
        timingWindows =       Nullable.Absent,
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
            rTimingWindows,
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
