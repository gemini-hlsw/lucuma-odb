// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package predicate

import edu.gemini.grackle.Path

trait Predicates[F[_]] extends BaseMapping[F] {

  /**
   * Module of predicates for top-level types. Start here (with whatever your result type is) when
   * constructing filters, etc.
   */
  object Predicates {
    val addConditionsEntyResult = AddConditionsEntryResultPredicates(Path.from(AddConditionsEntryResultType))
    val asterismGroup           = AsterismGroupPredicates(Path.from(AsterismGroupType))
    val cloneObservationResult  = CloneObservationResultPredicates(Path.from(CloneObservationResultType))
    val cloneTargetResult       = CloneTargetResultPredicates(Path.from(CloneTargetResultType))
    val constraintSetGroup      = ConstraintSetGroupPredicates(Path.from(ConstraintSetGroupType))
    val gmosNorthStep           = StepPredicates(Path.from(GmosNorthStepType))
    val gmosNorthVisit          = VisitPredicates(Path.from(GmosNorthVisitType))
    val gmosSouthStep           = StepPredicates(Path.from(GmosSouthStepType))
    val gmosSouthVisit          = VisitPredicates(Path.from(GmosSouthVisitType))
    val group                   = GroupPredicates(Path.from(GroupType))
    val groupEdit               = GroupEditPredicates(Path.from(GroupEditType))
    val groupElement            = GroupElementPredicates(Path.from(GroupElementType))
    val createGroupResult       = CreateGroupResultPredicates(Path.from(CreateGroupResultType))
    val linkUserResult          = LinkUserResultPredicates(Path.from(LinkUserResultType))
    val obsAttachment           = ObsAttachmentPredicates(Path.from(ObsAttachmentType))
    val observation             = ObservationPredicates(Path.from(ObservationType))
    val observationEdit         = ObservationEditPredicates(Path.from(ObservationEditType))
    val program                 = ProgramPredicates(Path.from(ProgramType))
    val programEdit             = ProgramEditPredicates(Path.from(ProgramEditType))
    val proposalAttachment      = ProposalAttachmentPredicates(Path.from(ProposalAttachmentType))
    val proposalClass           = ProposalClassPredicates(Path.from(ProposalClassType))
    val sequenceEvent           = ExecutionEventPredicates(Path.from(SequenceEventType))
    val setAllocationResult     = SetAllocationResultPredicates(Path.from(SetAllocationResultType))
    val stepEvent               = ExecutionEventPredicates(Path.from(StepEventType))
    val target                  = TargetPredicates(Path.from(TargetType))
    val targetEdit              = TargetEditPredicates(Path.from(TargetEditType))
    val targetGroup             = TargetGroupPredicates(Path.from(TargetGroupType))
  }

}
