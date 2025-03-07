// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import grackle.circe.CirceMappingLike
import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.util.MappingExtras
import lucuma.odb.graphql.util.SchemaSemigroup

trait BaseMapping[F[_]]
  extends SkunkMapping[F]
     with SchemaSemigroup[F]
     with MappingExtras[F]
     with CirceMappingLike[F] {

  // TODO: auto-generate this
  lazy val AddAtomEventResultType                  = schema.ref("AddAtomEventResult")
  lazy val AddConditionsEntryResultType            = schema.ref("AddConditionsEntryResult")
  lazy val AddDatasetEventResultType               = schema.ref("AddDatasetEventResult")
  lazy val AddProgramUserResultType                = schema.ref("AddProgramUserResult")
  lazy val AddSequenceEventResultType              = schema.ref("AddSequenceEventResult")
  lazy val AddSlewEventResultType                  = schema.ref("AddSlewEventResult")
  lazy val AddStepEventResultType                  = schema.ref("AddStepEventResult")
  lazy val AddTimeChargeCorrectionResultType       = schema.ref("AddTimeChargeCorrectionResult")
  lazy val AirMassRangeType                        = schema.ref("AirMassRange")
  lazy val AllocationType                          = schema.ref("Allocation")
  lazy val AsterismGroupType                       = schema.ref("AsterismGroup")
  lazy val AsterismGroupSelectResultType           = schema.ref("AsterismGroupSelectResult")
  lazy val AngleType                               = schema.ref("Angle")
  lazy val AtomExecutionStateType                  = schema.ref("AtomExecutionState")
  lazy val AtomEventType                           = schema.ref("AtomEvent")
  lazy val AtomIdType                              = schema.ref("AtomId")
  lazy val AtomRecordType                          = schema.ref("AtomRecord")
  lazy val AtomRecordSelectResultType              = schema.ref("AtomRecordSelectResult")
  lazy val AtomStageType                           = schema.ref("AtomStage")
  lazy val AttachmentIdType                        = schema.ref("AttachmentId")
  lazy val AttachmentType                          = schema.ref("Attachment")
  lazy val AttachmentTypeType                      = schema.ref("AttachmentType")
  lazy val BiasType                                = schema.ref("Bias")
  lazy val BigDecimalType                          = schema.ref("BigDecimal")
  lazy val CalibrationProgramReferenceType         = schema.ref("CalibrationProgramReference")
  lazy val CalibrationRoleType                     = schema.ref("CalibrationRole")
  lazy val CallForProposalsType                    = schema.ref("CallForProposals")
  lazy val CallForProposalsIdType                  = schema.ref("CallForProposalsId")
  lazy val CallForProposalsPartnerType             = schema.ref("CallForProposalsPartner")
  lazy val CallsForProposalsSelectResultType       = schema.ref("CallsForProposalsSelectResult")
  lazy val CallForProposalsTypeType                = schema.ref("CallForProposalsType")
  lazy val CategorizedTimeType                     = schema.ref("CategorizedTime")
  lazy val CatalogInfoType                         = schema.ref("CatalogInfo")
  lazy val CatalogNameType                         = schema.ref("CatalogName")
  lazy val ChangeProgramUserRoleResultType         = schema.ref("ChangeProgramUserRoleResult")
  lazy val ChargeClassType                         = schema.ref("ChargeClass")
  lazy val ChronicleIdType                         = schema.ref("ChronicleId")
  lazy val ClassicalType                           = schema.ref("Classical")
  lazy val CloneGroupResultType                    = schema.ref("CloneGroupResult")
  lazy val CloneObservationResultType              = schema.ref("CloneObservationResult")
  lazy val CloneTargetResultType                   = schema.ref("CloneTargetResult")
  lazy val CloudExtinctionType                     = schema.ref("CloudExtinction")
  lazy val CommissioningProgramReferenceType       = schema.ref("CommissioningProgramReference")
  lazy val ConditionsEntryType                     = schema.ref("ConditionsEntry")
  lazy val ConditionsMeasurementType               = schema.ref("ConditionsMeasurement")
  lazy val ConditionsMeasurementSourceType         = schema.ref("ConditionsMeasurementSource")
  lazy val ConditionsIntuitionType                 = schema.ref("ConditionsIntuition")
  lazy val ConditionsExpectationType               = schema.ref("ConditionsExpectation")
  lazy val ConditionsExpectationTypeType           = schema.ref("ConditionsExpectationType")
  lazy val ConfigurationConditionsType             = schema.ref("ConfigurationConditions")
  lazy val ConfigurationGmosNorthLongSlitType      = schema.ref("ConfigurationGmosNorthLongSlit")
  lazy val ConfigurationGmosSouthLongSlitType      = schema.ref("ConfigurationGmosSouthLongSlit")
  lazy val ConfigurationObservingModeType          = schema.ref("ConfigurationObservingMode")
  lazy val ConfigurationType                       = schema.ref("Configuration")
  lazy val ConfigurationRequestType                = schema.ref("ConfigurationRequest")
  lazy val ConfigurationRequestEditType            = schema.ref("ConfigurationRequestEdit")
  lazy val ConfigurationRequestIdType              = schema.ref("ConfigurationRequestId")
  lazy val ConfigurationRequestStatusType          = schema.ref("ConfigurationRequestStatus")
  lazy val ConstraintSetType                       = schema.ref("ConstraintSet")
  lazy val ConstraintSetGroupType                  = schema.ref("ConstraintSetGroup")
  lazy val ConstraintSetGroupSelectResultType      = schema.ref("ConstraintSetGroupSelectResult")
  lazy val CoordinateLimitsType                    = schema.ref("CoordinateLimits")
  lazy val CoordinatesType                         = schema.ref("Coordinates")
  lazy val CreateCallForProposalsResultType        = schema.ref("CreateCallForProposalsResult")
  lazy val CreateGroupResultType                   = schema.ref("CreateGroupResult")
  lazy val CreateObservationResultType             = schema.ref("CreateObservationResult")
  lazy val CreateProgramNoteResultType             = schema.ref("CreateProgramNoteResult")
  lazy val CreateProgramResultType                 = schema.ref("CreateProgramResult")
  lazy val CreateProposalResultType                = schema.ref("CreateProposalResult")
  lazy val CreateTargetResultType                  = schema.ref("CreateTargetResult")
  lazy val CreateUserInvitationResultType          = schema.ref("CreateUserInvitationResult")
  lazy val DarkType                                = schema.ref("Dark")
  lazy val DatasetEditType                         = schema.ref("DatasetEdit")
  lazy val DatasetEventType                        = schema.ref("DatasetEvent")
  lazy val DatasetIdType                           = schema.ref("DatasetId")
  lazy val DatasetFilenameType                     = schema.ref("DatasetFilename")
  lazy val DatasetQaStateType                      = schema.ref("DatasetQaState")
  lazy val DatasetReferenceLabelType               = schema.ref("DatasetReferenceLabel")
  lazy val DatasetReferenceType                    = schema.ref("DatasetReference")
  lazy val DatasetSelectResultType                 = schema.ref("DatasetSelectResult")
  lazy val DatasetStageType                        = schema.ref("DatasetStage")
  lazy val DatasetType                             = schema.ref("Dataset")
  lazy val DateType                                = schema.ref("Date")
  lazy val DeclinationType                         = schema.ref("Declination")
  lazy val DeleteProgramUserResultType             = schema.ref("DeleteProgramUserResult")
  lazy val DeleteProposalResultType                = schema.ref("DeleteProposalResult")
  lazy val DemoScienceType                         = schema.ref("DemoScience")
  lazy val DirectorsTimeType                       = schema.ref("DirectorsTime")
  lazy val DmsStringType                           = schema.ref("DmsString")
  lazy val EditTypeType                            = schema.ref("EditType")
  lazy val EducationalStatusType                   = schema.ref("EducationalStatus")
  lazy val EmailAddressType                        = schema.ref("EmailAddress")
  lazy val EmailStatusType                         = schema.ref("EmailStatus")
  lazy val EmailType                               = schema.ref("Email")
  lazy val ElevationRangeType                      = schema.ref("ElevationRange")
  lazy val EngineeringProgramReferenceType         = schema.ref("EngineeringProgramReference")
  lazy val EphemerisKeyTypeType                    = schema.ref("EphemerisKeyType")
  lazy val EpochStringType                         = schema.ref("EpochString")
  lazy val ExampleProgramReferenceType             = schema.ref("ExampleProgramReference")
  lazy val ExecutionEventAddedType                 = schema.ref("ExecutionEventAdded")
  lazy val ExecutionEventIdType                    = schema.ref("ExecutionEventId")
  lazy val ExecutionType                           = schema.ref("Execution")
  lazy val ExecutionEventType                      = schema.ref("ExecutionEvent")
  lazy val ExecutionEventTypeType                  = schema.ref("ExecutionEventType")
  lazy val ExecutionEventSelectResultType          = schema.ref("ExecutionEventSelectResult")
  lazy val ExecutionStateType                      = schema.ref("ExecutionState")
  lazy val ExistenceType                           = schema.ref("Existence")
  lazy val ExposureTimeModeType                    = schema.ref("ExposureTimeMode")
  lazy val ExtinctionType                          = schema.ref("Extinction")
  lazy val FastTurnaroundType                      = schema.ref("FastTurnaround")
  lazy val GenderType                              = schema.ref("Gender")
  lazy val F2FpuType                               = schema.ref("Flamingos2Fpu")
  lazy val F2DisperserType                         = schema.ref("Flamingos2Disperser")
  lazy val F2FilterType                            = schema.ref("Flamingos2Filter")
  lazy val FilterTypeMetaType                      = schema.ref("FilterTypeMeta")
  lazy val FilterTypeType                          = schema.ref("FilterType")
  lazy val FocalPlaneType                          = schema.ref("FocalPlane")
  lazy val GcalArcType                             = schema.ref("GcalArc")
  lazy val GcalContinuumType                       = schema.ref("GcalContinuum")
  lazy val GcalDiffuserType                        = schema.ref("GcalDiffuser")
  lazy val GcalFilterType                          = schema.ref("GcalFilter")
  lazy val GcalShutterType                         = schema.ref("GcalShutter")
  lazy val GcalType                                = schema.ref("Gcal")
  lazy val GmosAmpCountType                        = schema.ref("GmosAmpCount")
  lazy val GmosAmpGainType                         = schema.ref("GmosAmpGain")
  lazy val GmosAmpReadModeType                     = schema.ref("GmosAmpReadMode")
  lazy val GmosCcdModeType                         = schema.ref("GmosCcdMode")
  lazy val GmosCustomMaskType                      = schema.ref("GmosCustomMask")
  lazy val GmosCustomSlitWidthType                 = schema.ref("GmosCustomSlitWidth")
  lazy val GmosDtaxType                            = schema.ref("GmosDtax")
  lazy val GmosGratingOrderType                    = schema.ref("GmosGratingOrder")
  lazy val GmosNorthAtomType                       = schema.ref("GmosNorthAtom")
  lazy val GmosNorthBuiltinFpuType                 = schema.ref("GmosNorthBuiltinFpu")
  lazy val GmosNorthDetectorType                   = schema.ref("GmosNorthDetector")
  lazy val GmosNorthDynamicType                    = schema.ref("GmosNorthDynamic")
  lazy val GmosNorthExecutionConfigType            = schema.ref("GmosNorthExecutionConfig")
  lazy val GmosNorthFilterType                     = schema.ref("GmosNorthFilter")
  lazy val GmosNorthFpuType                        = schema.ref("GmosNorthFpu")
  lazy val GmosNorthGratingConfigType              = schema.ref("GmosNorthGratingConfig")
  lazy val GmosNorthGratingType                    = schema.ref("GmosNorthGrating")
  lazy val GmosNorthLongSlitType                   = schema.ref("GmosNorthLongSlit")
  lazy val GmosNorthStageModeType                  = schema.ref("GmosNorthStageMode")
  lazy val GmosNorthStaticType                     = schema.ref("GmosNorthStatic")
  lazy val GmosNorthStepType                       = schema.ref("GmosNorthStep")
  lazy val GmosRoiType                             = schema.ref("GmosRoi")
  lazy val GmosSouthAtomType                       = schema.ref("GmosSouthAtom")
  lazy val GmosSouthBuiltinFpuType                 = schema.ref("GmosSouthBuiltinFpu")
  lazy val GmosSouthDetectorType                   = schema.ref("GmosSouthDetector")
  lazy val GmosSouthDynamicType                    = schema.ref("GmosSouthDynamic")
  lazy val GmosSouthExecutionConfigType            = schema.ref("GmosSouthExecutionConfig")
  lazy val GmosSouthFilterType                     = schema.ref("GmosSouthFilter")
  lazy val GmosSouthFpuType                        = schema.ref("GmosSouthFpu")
  lazy val GmosSouthGratingConfigType              = schema.ref("GmosSouthGratingConfig")
  lazy val GmosSouthGratingType                    = schema.ref("GmosSouthGrating")
  lazy val GmosSouthLongSlitType                   = schema.ref("GmosSouthLongSlit")
  lazy val GmosSouthStageModeType                  = schema.ref("GmosSouthStageMode")
  lazy val GmosSouthStaticType                     = schema.ref("GmosSouthStatic")
  lazy val GmosSouthStepType                       = schema.ref("GmosSouthStep")
  lazy val GmosXBinningType                        = schema.ref("GmosXBinning")
  lazy val GmosYBinningType                        = schema.ref("GmosYBinning")
  lazy val GoaPropertiesType                       = schema.ref("GoaProperties")
  lazy val GroupType                               = schema.ref("Group")
  lazy val GroupIdType                             = schema.ref("GroupId")
  lazy val GroupEditType                           = schema.ref("GroupEdit")
  lazy val GroupElementType                        = schema.ref("GroupElement")
  lazy val GuideProbeType                          = schema.ref("GuideProbe")
  lazy val GuideStateType                          = schema.ref("GuideState")
  lazy val HasNonPartnerType                       = schema.ref("HasNonPartner")
  lazy val HasPartnerType                          = schema.ref("HasPartner")
  lazy val HasUnspecifiedPartnerType               = schema.ref("HasUnspecifiedPartner")
  lazy val HmsStringType                           = schema.ref("HmsString")
  lazy val HourAngleRangeType                      = schema.ref("HourAngleRange")
  lazy val ImageQualityType                        = schema.ref("ImageQuality")
  lazy val InstrumentType                          = schema.ref("Instrument")
  lazy val IntPercentType                          = schema.ref("IntPercent")
  lazy val LargeProgramType                        = schema.ref("LargeProgram")
  lazy val LibraryProgramReferenceType             = schema.ref("LibraryProgramReference")
  lazy val LinkUserResultType                      = schema.ref("LinkUserResult")
  lazy val LongType                                = schema.ref("Long")
  lazy val MonitoringProgramReferenceType          = schema.ref("MonitoringProgramReference")
  lazy val MosPreImagingType                       = schema.ref("MosPreImaging")
  lazy val MutationType                            = schema.ref("Mutation")
  lazy val NonEmptyStringType                      = schema.ref("NonEmptyString")
  lazy val NonNegIntType                           = schema.ref("NonNegInt")
  lazy val NonNegShortType                         = schema.ref("NonNegShort")
  lazy val NonsiderealType                         = schema.ref("Nonsidereal")
  lazy val OffsetPType                             = schema.ref("OffsetP")
  lazy val OffsetQType                             = schema.ref("OffsetQ")
  lazy val OffsetType                              = schema.ref("Offset")
  lazy val ObservationEditType                     = schema.ref("ObservationEdit")
  lazy val ObservationIdType                       = schema.ref("ObservationId")
  lazy val ObservationType                         = schema.ref("Observation")
  lazy val ObservationReferenceType                = schema.ref("ObservationReference")
  lazy val ObservationReferenceLabelType           = schema.ref("ObservationReferenceLabel")
  lazy val ObservationSelectResultType             = schema.ref("ObservationSelectResult")
  lazy val ObserveClassType                        = schema.ref("ObserveClass")
  lazy val ObservingModeGroupType                  = schema.ref("ObservingModeGroup")
  lazy val ObservingModeGroupSelectResultType      = schema.ref("ObservingModeGroupSelectResult")
  lazy val ObservingModeType                       = schema.ref("ObservingMode")
  lazy val ObservingModeTypeType                   = schema.ref("ObservingModeType")
  lazy val ParallaxType                            = schema.ref("Parallax")
  lazy val PartnerMetaType                         = schema.ref("PartnerMeta")
  lazy val PartnerSplitType                        = schema.ref("PartnerSplit")
  lazy val PartnerType                             = schema.ref("Partner")
  lazy val PartnerLinkType                         = schema.ref("PartnerLink")
  lazy val PartnerLinkTypeType                     = schema.ref("PartnerLinkType")
  lazy val PoorWeatherType                         = schema.ref("PoorWeather")
  lazy val PosAngleConstraintModeType              = schema.ref("PosAngleConstraintMode")
  lazy val PosAngleConstraintType                  = schema.ref("PosAngleConstraint")
  lazy val PosBigDecimalType                       = schema.ref("PosBigDecimal")
  lazy val PosIntType                              = schema.ref("PosInt")
  lazy val ProgramEditType                         = schema.ref("ProgramEdit")
  lazy val ProgramIdType                           = schema.ref("ProgramId")
  lazy val ProgramNoteIdType                       = schema.ref("ProgramNoteId")
  lazy val ProgramNoteSelectResultType             = schema.ref("ProgramNoteSelectResult")
  lazy val ProgramNoteType                         = schema.ref("ProgramNote")
  lazy val ProgramReferenceType                    = schema.ref("ProgramReference")
  lazy val ProgramReferenceLabelType               = schema.ref("ProgramReferenceLabel")
  lazy val ProgramSelectResultType                 = schema.ref("ProgramSelectResult")
  lazy val ProgramUserSelectResultType             = schema.ref("ProgramUserSelectResult")
  lazy val ProgramType                             = schema.ref("Program")
  lazy val ProgramTypeType                         = schema.ref("ProgramType")
  lazy val ProgramUserRoleType                     = schema.ref("ProgramUserRole")
  lazy val ProgramUserSupportRoleTypeType          = schema.ref("ProgramUserSupportRoleType")
  lazy val ProgramUserType                         = schema.ref("ProgramUser")
  lazy val ProgramUserIdType                       = schema.ref("ProgramUserId")
  lazy val ProperMotionDeclinationType             = schema.ref("ProperMotionDeclination")
  lazy val ProperMotionRaType                      = schema.ref("ProperMotionRA")
  lazy val ProperMotionType                        = schema.ref("ProperMotion")
  lazy val ProposalReferenceType                   = schema.ref("ProposalReference")
  lazy val ProposalReferenceLabelType              = schema.ref("ProposalReferenceLabel")
  lazy val ProposalStatusMetaType                  = schema.ref("ProposalStatusMeta")
  lazy val ProposalStatusType                      = schema.ref("ProposalStatus")
  lazy val ProposalType                            = schema.ref("Proposal")
  lazy val ProposalTypeType                        = schema.ref("ProposalType")
  lazy val QueryType                               = schema.ref("Query")
  lazy val QueueType                               = schema.ref("Queue")
  lazy val RadialVelocityType                      = schema.ref("RadialVelocity")
  lazy val RecordAtomResultType                    = schema.ref("RecordAtomResult")
  lazy val RecordDatasetResultType                 = schema.ref("RecordDatasetResult")
  lazy val RecordGmosNorthStepResultType           = schema.ref("RecordGmosNorthStepResult")
  lazy val RecordGmosNorthVisitResultType          = schema.ref("RecordGmosNorthVisitResult")
  lazy val RecordGmosSouthStepResultType           = schema.ref("RecordGmosSouthStepResult")
  lazy val RecordGmosSouthVisitResultType          = schema.ref("RecordGmosSouthVisitResult")
  lazy val RedeemUserInvitationResultType          = schema.ref("RedeemUserInvitationResult")
  lazy val RevokeUserInvitationResultType          = schema.ref("RevokeUserInvitationResult")
  lazy val RightAscensionType                      = schema.ref("RightAscension")
  lazy val ScienceBandType                         = schema.ref("ScienceBand")
  lazy val ScienceModeType                         = schema.ref("ScienceMode")
  lazy val ScienceProgramReferenceType             = schema.ref("ScienceProgramReference")
  lazy val ScienceRequirementsType                 = schema.ref("ScienceRequirements")
  lazy val ScienceSubtypeType                      = schema.ref("ScienceSubtype")
  lazy val ScienceType                             = schema.ref("Science")
  lazy val SeeingTrendType                         = schema.ref("SeeingTrend")
  lazy val SemesterType                            = schema.ref("Semester")
  lazy val SequenceCommandType                     = schema.ref("SequenceCommand")
  lazy val SequenceEventType                       = schema.ref("SequenceEvent")
  lazy val SequenceTypeType                        = schema.ref("SequenceType")
  lazy val SetAllocationsResultType                = schema.ref("SetAllocationsResult")
  lazy val SetGuideTargetNameResultType            = schema.ref("SetGuideTargetNameResult")
  lazy val SetProgramReferenceResultType           = schema.ref("SetProgramReferenceResult")
  lazy val SetProposalStatusResultType             = schema.ref("SetProposalStatusResult")
  lazy val SiderealType                            = schema.ref("Sidereal")
  lazy val SignalToNoiseType                       = schema.ref("SignalToNoise")
  lazy val SignalToNoiseExposureTimeModeType       = schema.ref("SignalToNoiseExposureTimeMode")
  lazy val SiteCoordinateLimitsType                = schema.ref("SiteCoordinateLimits")
  lazy val SiteType                                = schema.ref("Site")
  lazy val SkyBackgroundType                       = schema.ref("SkyBackground")
  lazy val SlewEventType                           = schema.ref("SlewEvent")
  lazy val SlewStageType                           = schema.ref("SlewStage")
  lazy val SmartGcalType                           = schema.ref("SmartGcal")
  lazy val SmartGcalTypeType                       = schema.ref("SmartGcalType")
  lazy val SpectroscopyCapabilitiesType            = schema.ref("SpectroscopyCapabilities")
  lazy val SpectroscopyConfigOptionF2Type          = schema.ref("SpectroscopyConfigOptionFlamingos2")
  lazy val SpectroscopyConfigOptionGmosNorthType   = schema.ref("SpectroscopyConfigOptionGmosNorth")
  lazy val SpectroscopyConfigOptionGmosSouthType   = schema.ref("SpectroscopyConfigOptionGmosSouth")
  lazy val SpectroscopyConfigOptionType            = schema.ref("SpectroscopyConfigOption")
  lazy val SpectroscopyScienceRequirementsType     = schema.ref("SpectroscopyScienceRequirements")
  lazy val StepConfigType                          = schema.ref("StepConfig")
  lazy val StepEventType                           = schema.ref("StepEvent")
  lazy val StepExecutionStateType                  = schema.ref("StepExecutionState")
  lazy val StepIdType                              = schema.ref("StepId")
  lazy val StepRecordType                          = schema.ref("StepRecord")
  lazy val StepRecordSelectResultType              = schema.ref("StepRecordSelectResult")
  lazy val StepStageType                           = schema.ref("StepStage")
  lazy val StepTypeType                            = schema.ref("StepType")
  lazy val SystemProgramReferenceType              = schema.ref("SystemProgramReference")
  lazy val SystemVerificationType                  = schema.ref("SystemVerification")
  lazy val TacCategoryType                         = schema.ref("TacCategory")
  lazy val TargetEditType                          = schema.ref("TargetEdit")
  lazy val TargetEnvironmentType                   = schema.ref("TargetEnvironment")
  lazy val TargetGroupSelectResultType             = schema.ref("TargetGroupSelectResult")
  lazy val TargetGroupType                         = schema.ref("TargetGroup")
  lazy val TargetIdType                            = schema.ref("TargetId")
  lazy val TargetSelectResultType                  = schema.ref("TargetSelectResult")
  lazy val TargetType                              = schema.ref("Target")
  lazy val TelescopeConfig                         = schema.ref("TelescopeConfig")
  lazy val TimeAccountingCategoryType              = schema.ref("TimeAccountingCategory")
  lazy val TimeAndCountExposureTimeModeType        = schema.ref("TimeAndCountExposureTimeMode")
  lazy val TimeChargeCorrectionType                = schema.ref("TimeChargeCorrection")
  lazy val TimeChargeCorrectionOpType              = schema.ref("TimeChargeCorrectionOp")
  lazy val TimeChargeDaylightDiscountType          = schema.ref("TimeChargeDaylightDiscount")
  lazy val TimeChargeDiscountType                  = schema.ref("TimeChargeDiscount")
  lazy val TimeChargeInvoiceType                   = schema.ref("TimeChargeInvoice")
  lazy val TimeChargeNoDataDiscountType            = schema.ref("TimeChargeNoDataDiscount")
  lazy val TimeChargeQaDiscountType                = schema.ref("TimeChargeQaDiscount")
  lazy val TimeSpanType                            = schema.ref("TimeSpan")
  lazy val TimestampType                           = schema.ref("Timestamp")
  lazy val TimestampIntervalType                   = schema.ref("TimestampInterval")
  lazy val TimingWindowEndAfterType                = schema.ref("TimingWindowEndAfter")
  lazy val TimingWindowEndAtType                   = schema.ref("TimingWindowEndAt")
  lazy val TimingWindowEndType                     = schema.ref("TimingWindowEnd")
  lazy val TimingWindowInclusionType               = schema.ref("TimingWindowInclusion")
  lazy val TimingWindowRepeatType                  = schema.ref("TimingWindowRepeat")
  lazy val TimingWindowType                        = schema.ref("TimingWindow")
  lazy val ToOActivationType                       = schema.ref("ToOActivation")
  lazy val TransactionIdType                       = schema.ref("TransactionId")
  lazy val UnlinkUserResultType                    = schema.ref("UnlinkUserResult")
  lazy val UpdateAsterismsResultType               = schema.ref("UpdateAsterismsResult")
  lazy val UpdateAttachmentsResultType             = schema.ref("UpdateAttachmentsResult")
  lazy val UpdateCallsForProposalsResultType       = schema.ref("UpdateCallsForProposalsResult")
  lazy val UpdateConfigurationRequestsResultType   = schema.ref("UpdateConfigurationRequestsResult")
  lazy val UpdateDatasetsResultType                = schema.ref("UpdateDatasetsResult")
  lazy val UpdateGroupsResultType                  = schema.ref("UpdateGroupsResult")
  lazy val UpdateObservationsResultType            = schema.ref("UpdateObservationsResult")
  lazy val UpdateProgramsResultType                = schema.ref("UpdateProgramsResult")
  lazy val UpdateProgramNotesResultType            = schema.ref("UpdateProgramNotesResult")
  lazy val UpdateProgramUsersResultType            = schema.ref("UpdateProgramUsersResult")
  lazy val UpdateProposalResultType                = schema.ref("UpdateProposalResult")
  lazy val UpdateTargetsResultType                 = schema.ref("UpdateTargetsResult")
  lazy val UserIdType                              = schema.ref("UserId")
  lazy val UserInvitationIdType                    = schema.ref("UserInvitationId")
  lazy val UserInvitationKeyType                   = schema.ref("UserInvitationKey")
  lazy val UserInvitationType                      = schema.ref("UserInvitation")
  lazy val UserInvitationStatusType                = schema.ref("UserInvitationStatus")
  lazy val UserProfileType                         = schema.ref("UserProfile")
  lazy val UserType                                = schema.ref("User")
  lazy val UserTypeType                            = schema.ref("UserType")
  lazy val VisitType                               = schema.ref("Visit")
  lazy val VisitIdType                             = schema.ref("VisitId")
  lazy val VisitSelectResultType                   = schema.ref("VisitSelectResult")
  lazy val WaterVaporType                          = schema.ref("WaterVapor")
  lazy val WavelengthType                          = schema.ref("Wavelength")

}
