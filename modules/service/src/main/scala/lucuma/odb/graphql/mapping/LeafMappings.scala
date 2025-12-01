// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import io.circe.syntax.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.*
import lucuma.core.math.Epoch
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Attachment
import lucuma.core.model.CallForProposals
import lucuma.core.model.Client
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Group
import lucuma.core.model.ImageQuality
import lucuma.core.model.IntPercent
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.Program
import lucuma.core.model.ProgramNote
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProgramUser
import lucuma.core.model.ProposalReference
import lucuma.core.model.Semester
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.UserInvitation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.DatasetReference
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.CalculationState
import lucuma.core.util.IdempotencyKey
import lucuma.core.util.Timestamp
import lucuma.odb.data.AtomExecutionState
import lucuma.odb.data.BlindOffsetType
import lucuma.odb.data.DatabaseOperation
import lucuma.odb.data.EditType
import lucuma.odb.data.ExecutionEventType
import lucuma.odb.data.Existence
import lucuma.odb.data.Extinction
import lucuma.odb.data.GmosImagingVariantType
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.data.StepExecutionState
import lucuma.odb.data.Tag
import lucuma.odb.data.UserType
import lucuma.odb.data.WavelengthOrder
import lucuma.odb.sequence.data.OffsetGeneratorType

import java.time.LocalDate

trait LeafMappings[F[_]] extends BaseMapping[F] {

  private given io.circe.Encoder[Epoch] =
    e => Json.fromString(Epoch.fromString.reverseGet(e))

  // TODO: move
  private given io.circe.Encoder[Semester] =
    _.format.asJson

  lazy val LeafMappings: List[TypeMapping] =
    List(
      LeafMapping[ArcType](ArcTypeType),
      LeafMapping[Atom.Id](AtomIdType),
      LeafMapping[AtomExecutionState](AtomExecutionStateType),
      LeafMapping[AtomStage](AtomStageType),
      LeafMapping[Attachment.Id](AttachmentIdType),
      LeafMapping[AttachmentType](AttachmentTypeType),
      LeafMapping[BigDecimal](BigDecimalType),
      LeafMapping[BlindOffsetType](BlindOffsetTypeType),
      LeafMapping[CalibrationRole](CalibrationRoleType),
      LeafMapping[CallForProposals.Id](CallForProposalsIdType),
      LeafMapping[CallForProposalsType](CallForProposalsTypeType),
      LeafMapping[CatalogName](CatalogNameType),
      LeafMapping[ChargeClass](ChargeClassType),
      LeafMapping[Long](ChronicleIdType),
      LeafMapping[Client.Id](ClientIdType),
      LeafMapping[CloudExtinction.Preset](CloudExtinctionPresetType),
      LeafMapping[ConfigurationRequest.Id](ConfigurationRequestIdType),
      LeafMapping[ConfigurationRequestStatus](ConfigurationRequestStatusType),
      LeafMapping[DatabaseOperation](DatabaseOperationType),
      LeafMapping[DatasetQaState](DatasetQaStateType),
      LeafMapping[DatasetReference](DatasetReferenceLabelType),
      LeafMapping[Tag](ConditionsExpectationTypeType),
      LeafMapping[Tag](ConditionsMeasurementSourceType),
      LeafMapping[NonEmptyString](DatasetFilenameType),
      LeafMapping[Dataset.Id](DatasetIdType),
      LeafMapping[DatasetStage](DatasetStageType),
      LeafMapping[LocalDate](DateType),
      LeafMapping[EditType](EditTypeType),
      LeafMapping[EducationalStatus](EducationalStatusType),
      LeafMapping[EmailAddress](EmailAddressType),
      LeafMapping[EmailStatus](EmailStatusType),
      LeafMapping[EphemerisKeyType](EphemerisKeyTypeType),
      LeafMapping[Epoch](EpochStringType),
      LeafMapping[ExecutionEvent.Id](ExecutionEventIdType),
      LeafMapping[ExecutionEventType](ExecutionEventTypeType),
      LeafMapping[ExecutionState](ExecutionStateType),
      LeafMapping[Existence](ExistenceType),
      LeafMapping[Extinction](ExtinctionType),
      LeafMapping[Tag](FilterTypeType),
      LeafMapping[Flamingos2CustomSlitWidth](Flamingos2CustomSlitWidthType),
      LeafMapping[Flamingos2Disperser](Flamingos2DisperserType),
      LeafMapping[Flamingos2Filter](Flamingos2FilterType),
      LeafMapping[Flamingos2Fpu](Flamingos2FpuType),
      LeafMapping[Flamingos2LyotWheel](Flamingos2LyotWheelType),
      LeafMapping[Flamingos2ReadMode](Flamingos2ReadModeType),
      LeafMapping[Flamingos2Decker](Flamingos2DeckerType),
      LeafMapping[Flamingos2ReadoutMode](Flamingos2ReadoutModeType),
      LeafMapping[Flamingos2Reads](Flamingos2ReadsType),
      LeafMapping[FocalPlane](FocalPlaneType),
      LeafMapping[Gender](GenderType),
      LeafMapping[GcalArc](GcalArcType),
      LeafMapping[GcalContinuum](GcalContinuumType),
      LeafMapping[GcalDiffuser](GcalDiffuserType),
      LeafMapping[GcalFilter](GcalFilterType),
      LeafMapping[GcalShutter](GcalShutterType),
      LeafMapping[GmosAmpCount](GmosAmpCountType),
      LeafMapping[GmosAmpGain](GmosAmpGainType),
      LeafMapping[GmosAmpReadMode](GmosAmpReadModeType),
      LeafMapping[GmosCustomSlitWidth](GmosCustomSlitWidthType),
      LeafMapping[GmosDtax](GmosDtaxType),
      LeafMapping[GmosGratingOrder](GmosGratingOrderType),
      LeafMapping[GmosImagingVariantType](GmosImagingVariantTypeType),
      LeafMapping[GmosLongSlitAcquisitionRoi](GmosLongSlitAcquisitionRoiType),
      LeafMapping[GmosNorthFpu](GmosNorthBuiltinFpuType),
      LeafMapping[GmosNorthDetector](GmosNorthDetectorType),
      LeafMapping[GmosNorthFilter](GmosNorthFilterType),
      LeafMapping[GmosNorthGrating](GmosNorthGratingType),
      LeafMapping[GmosNorthStageMode](GmosNorthStageModeType),
      LeafMapping[GmosSouthFpu](GmosSouthBuiltinFpuType),
      LeafMapping[GmosSouthDetector](GmosSouthDetectorType),
      LeafMapping[GmosSouthFilter](GmosSouthFilterType),
      LeafMapping[GmosSouthGrating](GmosSouthGratingType),
      LeafMapping[GmosSouthStageMode](GmosSouthStageModeType),
      LeafMapping[GmosRoi](GmosRoiType),
      LeafMapping[GmosBinning](GmosBinningType),
      LeafMapping[Group.Id](GroupIdType),
      LeafMapping[GuideProbe](GuideProbeType),
      LeafMapping[StepGuideState](GuideStateType),
      LeafMapping[IdempotencyKey](IdempotencyKeyType),
      LeafMapping[ImageQuality.Preset](ImageQualityPresetType),
      LeafMapping[Instrument](InstrumentType),
      LeafMapping[IntPercent](IntPercentType),
      LeafMapping[Long](LongType),
      LeafMapping[MosPreImaging](MosPreImagingType),
      LeafMapping[MultipleFiltersMode](MultipleFiltersModeType),
      LeafMapping[NonEmptyString](NonEmptyStringType),
      LeafMapping[NonNegInt](NonNegIntType),
      LeafMapping[NonNegShort](NonNegShortType),
      LeafMapping[CalculationState](CalculationStateType),
      LeafMapping[ObservingModeType](ObservingModeTypeType),
      LeafMapping[Observation.Id](ObservationIdType),
      LeafMapping[ObservationReference](ObservationReferenceLabelType),
      LeafMapping[ObservationWorkflowState](ObservationWorkflowStateType),
      LeafMapping[ObserveClass](ObserveClassType),
      LeafMapping[OffsetGeneratorType](OffsetGeneratorTypeType),
      LeafMapping[Partner](PartnerType),
      LeafMapping[PartnerLinkType](PartnerLinkTypeType),
      LeafMapping[PosAngleConstraintMode](PosAngleConstraintModeType),
      LeafMapping[PosBigDecimal](PosBigDecimalType),
      LeafMapping[PosInt](PosIntType),
      LeafMapping[Program.Id](ProgramIdType),
      LeafMapping[ProgramNote.Id](ProgramNoteIdType),
      LeafMapping[ProgramType](ProgramTypeType),
      LeafMapping[ProgramUser.Id](ProgramUserIdType),
      LeafMapping[ProgramUserRole](ProgramUserRoleType),
      LeafMapping[ProgramReference](ProgramReferenceLabelType),
      LeafMapping[ProposalReference](ProposalReferenceLabelType),
      LeafMapping[Tag](ProposalStatusType),
      LeafMapping[ScienceBand](ScienceBandType),
      LeafMapping[ScienceMode](ScienceModeType),
      LeafMapping[ScienceSubtype](ScienceSubtypeType),
      LeafMapping[Tag](SeeingTrendType),
      LeafMapping[Semester](SemesterType),
      LeafMapping[SequenceCommand](SequenceCommandType),
      LeafMapping[SequenceType](SequenceTypeType),
      LeafMapping[SignalToNoise](SignalToNoiseType),
      LeafMapping[Site](SiteType),
      LeafMapping[SkyBackground](SkyBackgroundType),
      LeafMapping[SlewStage](SlewStageType),
      LeafMapping[SmartGcalType](SmartGcalTypeType),
      LeafMapping[SpectroscopyCapabilities](SpectroscopyCapabilitiesType),
      LeafMapping[Step.Id](StepIdType),
      LeafMapping[StepExecutionState](StepExecutionStateType),
      LeafMapping[StepStage](StepStageType),
      LeafMapping[StepType](StepTypeType),
      LeafMapping[String](DmsStringType),
      LeafMapping[String](HmsStringType),
      LeafMapping[Tag](TacCategoryType),
      LeafMapping[Target.Id](TargetIdType),
      LeafMapping[TargetDisposition](TargetDispositionType),
      LeafMapping[TimeAccountingCategory](TimeAccountingCategoryType),
      LeafMapping[TimeChargeCorrection.Op](TimeChargeCorrectionOpType),
      LeafMapping[Timestamp](TimestampType),
      LeafMapping[TimingWindowInclusion](TimingWindowInclusionType),
      LeafMapping[ToOActivation](ToOActivationType),
      LeafMapping[Long](TransactionIdType),
      LeafMapping[User.Id](UserIdType),
      LeafMapping[String](UserInvitationKeyType),
      LeafMapping[UserInvitation.Id](UserInvitationIdType),
      LeafMapping[InvitationStatus](UserInvitationStatusType),
      LeafMapping[UserType](UserTypeType),
      LeafMapping[Visit.Id](VisitIdType),
      LeafMapping[WaterVapor](WaterVaporType),
      LeafMapping[WavelengthOrder](WavelengthOrderType)
    )

}
