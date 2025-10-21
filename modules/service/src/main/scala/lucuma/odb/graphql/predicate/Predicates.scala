// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package predicate

import grackle.Path

trait Predicates[F[_]] extends BaseMapping[F] {

  /**
   * Module of predicates for top-level types. Start here (with whatever your result type is) when
   * constructing filters, etc.
   */
  object Predicates {
    val addConditionsEntyResult       = AddConditionsEntryResultPredicates(Path.from(AddConditionsEntryResultType))
    val addTimeChargeCorrectionResult = AddTimeChargeCorrectionResultPredicates(Path.from(AddTimeChargeCorrectionResultType))
    val allocation                    = AllocationPredicates(Path.from(AllocationType))
    val asterismGroup                 = AsterismGroupPredicates(Path.from(AsterismGroupType))
    val atomEvent                     = ExecutionEventPredicates(Path.from(AtomEventType))
    val atomRecord                    = AtomRecordPredicates(Path.from(AtomRecordType))
    val attachment                    = AttachmentPredicates(Path.from(AttachmentType))
    val callForProposals              = CallForProposalsPredicates(Path.from(CallForProposalsType))
    val cloneGroupResult              = CloneGroupResultPredicates(Path.from(CloneGroupResultType))
    val cloneObservationResult        = CloneObservationResultPredicates(Path.from(CloneObservationResultType))
    val cloneTargetResult             = CloneTargetResultPredicates(Path.from(CloneTargetResultType))
    val configurationRequest          = ConfigurationRequestPredicates(Path.from(ConfigurationRequestType))
    val constraintSetGroup            = ConstraintSetGroupPredicates(Path.from(ConstraintSetGroupType))
    val createProposalResult          = CreateProposalResultPredicates(Path.from(CreateProposalResultType))
    val dataset                       = DatasetPredicates(Path.from(DatasetType))
    val datasetChronicleEntry         = DatasetChronicleEntryPredicates(Path.from(DatasetChronicleEntryType))
    val datasetEdit                   = DatasetEditPredicates(Path.from(DatasetEditType))
    val datasetEvent                  = ExecutionEventPredicates(Path.from(DatasetEventType))
    val executionEvent                = ExecutionEventPredicates(Path.from(ExecutionEventType))
    val executionEventAdded           = ExecutionEventAddedPredicates(Path.from(ExecutionEventAddedType))
    val exposureTimeMode              = ExposureTimeModePredicates(Path.from(ExposureTimeModeType))
    val flamingos2Step                = StepPredicates(Path.from(Flamingos2StepType))
    val gmosNorthStep                 = StepPredicates(Path.from(GmosNorthStepType))
    val gmosSouthStep                 = StepPredicates(Path.from(GmosSouthStepType))
    val group                         = GroupPredicates(Path.from(GroupType))
    val groupEdit                     = GroupEditPredicates(Path.from(GroupEditType))
    val groupElement                  = GroupElementPredicates(Path.from(GroupElementType))
    val createGroupResult             = CreateGroupResultPredicates(Path.from(CreateGroupResultType))
    val linkUserResult                = LinkUserResultPredicates(Path.from(LinkUserResultType))
    val obscalcUpdate                 = ObscalcUpdatePredicates(Path.from(ObscalcUpdateType))
    val observation                   = ObservationPredicates(Path.from(ObservationType))
    val observationEdit               = ObservationEditPredicates(Path.from(ObservationEditType))
    val observingModeGroup            = ObservingModeGroupPredicates(Path.from(ObservingModeGroupType))
    val program                       = ProgramPredicates(Path.from(ProgramType))
    val programEdit                   = ProgramEditPredicates(Path.from(ProgramEditType))
    val programNote                   = ProgramNotePredicates(Path.from(ProgramNoteType))
    val programUser                   = ProgramUserPredicates(Path.from(ProgramUserType))
    val proposal                      = ProposalPredicates(Path.from(ProposalType))
    val recordDatasetResult           = RecordDatasetResultPredicates(Path.from(RecordDatasetResultType))
    val resetAcquisitionResult        = ResetAcquisitionResultPredicates(Path.from(ResetAcquisitionResultType))
    val sequenceEvent                 = ExecutionEventPredicates(Path.from(SequenceEventType))
    val setGuideTargetNameResult      = SetGuideTargetNameResultPredicates(Path.from(SetGuideTargetNameResultType))
    val setProgramReferenceResult     = SetProgramReferenceResult(Path.from(SetProgramReferenceResultType))
    val setProposalStatusResult       = SetProposalStatusResultPredicates(Path.from(SetProposalStatusResultType))
    val slewEvent                     = ExecutionEventPredicates(Path.from(SlewEventType))
    val stepEvent                     = ExecutionEventPredicates(Path.from(StepEventType))
    val stepRecord                    = StepRecordPredicates(Path.from(StepRecordType))
    val target                        = TargetPredicates(Path.from(TargetType))
    val targetEdit                    = TargetEditPredicates(Path.from(TargetEditType))
    val targetGroup                   = TargetGroupPredicates(Path.from(TargetGroupType))
    val updateProposalResult          = UpdateProposalResultPredicates(Path.from(UpdateProposalResultType))
    val userInvitation                = UserInvitationPredicates(Path.from(UserInvitationType))
    val visit                         = VisitPredicates(Path.from(VisitType))
  }

}
