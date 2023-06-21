// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.Mapping
import eu.timepit.refined.types.numeric.NonNegBigDecimal
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.NonNegLong
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.*
import lucuma.core.enums.*
import lucuma.core.math.Epoch
import lucuma.core.math.SignalToNoise
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Group
import lucuma.core.model.IntPercent
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Step
import lucuma.core.util.Timestamp
import lucuma.odb.data.EditType
import lucuma.odb.data.Existence
import lucuma.odb.data.ObservingModeType
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.Tag
import lucuma.odb.data.UserType


trait LeafMappings[F[_]] extends BaseMapping[F] {

  private given io.circe.Encoder[Epoch] =
    e => Json.fromString(Epoch.fromString.reverseGet(e))

  lazy val LeafMappings: List[TypeMapping] =
    List(
      LeafMapping[BigDecimal](BigDecimalType),
      LeafMapping[CatalogName](CatalogNameType),
      LeafMapping[CloudExtinction](CloudExtinctionType),
      LeafMapping[Tag](ConditionsExpectationTypeType),
      LeafMapping[Tag](ConditionsSourceType),
      LeafMapping[EditType](EditTypeType),
      LeafMapping[EphemerisKeyType](EphemerisKeyTypeType),
      LeafMapping[Epoch](EpochStringType),
      LeafMapping[ExecutionEvent.Id](ExecutionEventIdType),
      LeafMapping[Existence](ExistenceType),
      LeafMapping[Tag](FilterTypeType),
      LeafMapping[FocalPlane](FocalPlaneType),
      LeafMapping[GcalArc](GcalArcType),
      LeafMapping[GcalContinuum](GcalContinuumType),
      LeafMapping[GcalDiffuser](GcalDiffuserType),
      LeafMapping[GcalFilter](GcalFilterType),
      LeafMapping[GcalShutter](GcalShutterType),
      LeafMapping[GmosNorthGrating](GmosNorthGratingType),
      LeafMapping[GmosAmpGain](GmosAmpGainType),
      LeafMapping[GmosAmpReadMode](GmosAmpReadModeType),
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
      LeafMapping[GmosXBinning](GmosXBinningType),
      LeafMapping[GmosYBinning](GmosYBinningType),
      LeafMapping[Group.Id](GroupIdType),
      LeafMapping[GuideState](GuideStateType),
      LeafMapping[ImageQuality](ImageQualityType),
      LeafMapping[Instrument](InstrumentType),
      LeafMapping[IntPercent](IntPercentType),
      LeafMapping[Long](LongType),
      LeafMapping[MosPreImaging](MosPreImagingType),
      LeafMapping[NonEmptyString](NonEmptyStringType),
      LeafMapping[NonNegBigDecimal](NonNegBigDecimalType),
      LeafMapping[NonNegLong](NonNegLongType),
      LeafMapping[NonNegShort](NonNegShortType),
      LeafMapping[ObsActiveStatus](ObsActiveStatusType),
      LeafMapping[ObsAttachment.Id](ObsAttachmentIdType),
      LeafMapping[Tag](ObsAttachmentTypeType),
      LeafMapping[ObservingModeType](ObservingModeTypeType),
      LeafMapping[Observation.Id](ObservationIdType),
      LeafMapping[ObsStatus](ObsStatusType),
      LeafMapping[Tag](PartnerType),
      LeafMapping[PosAngleConstraintMode](PosAngleConstraintModeType),
      LeafMapping[PosBigDecimal](PosBigDecimalType),
      LeafMapping[PosInt](PosIntType),
      LeafMapping[Program.Id](ProgramIdType),
      LeafMapping[ProgramUserRole](ProgramUserRoleType),
      LeafMapping[Tag](ProposalAttachmentTypeType),
      LeafMapping[ScienceMode](ScienceModeType),
      LeafMapping[Tag](SeeingTrendType),
      LeafMapping[SequenceCommand](SequenceCommandType),
      LeafMapping[SignalToNoise](SignalToNoiseType),
      LeafMapping[SkyBackground](SkyBackgroundType),
      LeafMapping[SmartGcalType](SmartGcalType),
      LeafMapping[SpectroscopyCapabilities](SpectroscopyCapabilitiesType),
      LeafMapping[Step.Id](StepIdType),
      LeafMapping[StepType](StepType),
      LeafMapping[String](DmsStringType),
      LeafMapping[String](HmsStringType),
      LeafMapping[Tag](TacCategoryType),
      LeafMapping[Target.Id](TargetIdType),
      LeafMapping[Timestamp](TimestampType),
      LeafMapping[TimingWindowInclusion](TimingWindowInclusionType),
      LeafMapping[ToOActivation](ToOActivationType),
      LeafMapping[User.Id](UserIdType),
      LeafMapping[UserType](UserTypeType),
      LeafMapping[Visit.Id](VisitIdType),
      LeafMapping[WaterVapor](WaterVaporType),
    )

}
