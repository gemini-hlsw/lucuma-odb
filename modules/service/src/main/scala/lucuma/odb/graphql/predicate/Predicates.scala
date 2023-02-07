// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package predicate

import edu.gemini.grackle.Path
import lucuma.odb.graphql.mapping.*

trait Predicates[F[_]] extends BaseMapping[F] {

  /**
   * Module of predicates for top-level types. Start here (with whatever your result type is) when
   * constructing filters, etc.
   */
  object Predicates {
    val asterismGroup       = AsterismGroupPredicates(Path.from(AsterismGroupType))
    val cloneTargetResult   = CloneTargetResultPredicates(Path.from(CloneTargetResultType))
    val constraintSetGroup  = ConstraintSetGroupPredicates(Path.from(ConstraintSetGroupType))
    val linkUserResult      = LinkUserResultPredicates(Path.from(LinkUserResultType))
    val observation         = ObservationPredicates(Path.from(ObservationType))
    val observationEdit     = ObservationEditPredicates(Path.from(ObservationEditType))
    val program             = ProgramPredicates(Path.from(ProgramType))
    val programEdit         = ProgramEditPredicates(Path.from(ProgramEditType))
    val proposalClass       = ProposalClassPredicates(Path.from(ProposalClassType))
    val setAllocationResult = SetAllocationResultPredicates(Path.from(SetAllocationResultType))
    val target              = TargetPredicates(Path.from(TargetType))
    val targetEdit          = TargetEditPredicates(Path.from(TargetEditType))
    val targetGroup         = TargetGroupPredicates(Path.from(TargetGroupType))
  }

}