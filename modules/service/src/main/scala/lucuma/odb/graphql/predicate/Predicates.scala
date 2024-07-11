// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
    val callForProposals              = CallForProposalsPredicates(Path.from(CallForProposalsType))
    val cloneObservationResult        = CloneObservationResultPredicates(Path.from(CloneObservationResultType))
    val cloneTargetResult             = CloneTargetResultPredicates(Path.from(CloneTargetResultType))
    val constraintSetGroup            = ConstraintSetGroupPredicates(Path.from(ConstraintSetGroupType))
    val createProposalResult          = CreateProposalResultPredicates(Path.from(CreateProposalResultType))
    val dataset                       = DatasetPredicates(Path.from(DatasetType))
    val datasetEvent                  = ExecutionEventPredicates(Path.from(DatasetEventType))
    val executionEvent                = ExecutionEventPredicates(Path.from(ExecutionEventType))
    val gmosNorthStep                 = StepPredicates(Path.from(GmosNorthStepType))
    val gmosSouthStep                 = StepPredicates(Path.from(GmosSouthStepType))
    val group                         = GroupPredicates(Path.from(GroupType))
    val groupEdit                     = GroupEditPredicates(Path.from(GroupEditType))
    val groupElement                  = GroupElementPredicates(Path.from(GroupElementType))
    val createGroupResult             = CreateGroupResultPredicates(Path.from(CreateGroupResultType))
    val linkUserResult                = LinkUserResultPredicates(Path.from(LinkUserResultType))
    val obsAttachment                 = ObsAttachmentPredicates(Path.from(ObsAttachmentType))
    val observation                   = ObservationPredicates(Path.from(ObservationType))
    val observationEdit               = ObservationEditPredicates(Path.from(ObservationEditType))
    val program                       = ProgramPredicates(Path.from(ProgramType))
    val programEdit                   = ProgramEditPredicates(Path.from(ProgramEditType))
    val proposal                      = ProposalPredicates(Path.from(ProposalType))
    val proposalAttachment            = ProposalAttachmentPredicates(Path.from(ProposalAttachmentType))
    val recordDatasetResult           = RecordDatasetResultPredicates(Path.from(RecordDatasetResultType))
    val sequenceEvent                 = ExecutionEventPredicates(Path.from(SequenceEventType))
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
