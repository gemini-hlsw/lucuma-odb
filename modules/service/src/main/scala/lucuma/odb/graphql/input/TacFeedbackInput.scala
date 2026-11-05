// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Partner
import lucuma.core.enums.TacPartnerDecision
import lucuma.core.syntax.string.*
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

/**
 * A partner's TAC feedback.  The decision and ranking are required to create
 * new feedback, but may be omitted when updating existing feedback in order to
 * keep their current values.  When supplied, the recommended times (at most one
 * per science band) replace all existing ones; when omitted they are kept.
 * GraphQL cannot express "optional but not nullable", so explicit nulls for
 * these are rejected by the binding.  Comments are optional: omitting them
 * keeps the current value (if any) and an explicit null removes them.
 */
case class TacFeedbackInput(
  partner:          Partner,
  decision:         Option[TacPartnerDecision],
  ranking:          Option[PosBigDecimal],
  recommendedTimes: Option[List[TacRecommendedTimeInput]],
  comments:         Nullable[NonEmptyString]
)

object TacFeedbackInput:

  val Binding: Matcher[TacFeedbackInput] =
    ObjectFieldsBinding.rmap:
      case List(
        PartnerBinding("partner", rPartner),
        TacPartnerDecisionBinding.NonNullable("decision", rDecision),
        PosBigDecimalBinding.NonNullable("ranking", rRanking),
        TacRecommendedTimeInput.Binding.List.NonNullable("recommendedTimes", rRecommendedTimes),
        NonEmptyStringBinding.Nullable("comments", rComments)
      ) =>
        val rRecommendedTimesʹ =
          mapDedup("recommendedTimes band", rRecommendedTimes)(_.band, _.tag.toScreamingSnakeCase)
        (rPartner, rDecision, rRanking, rRecommendedTimesʹ, rComments).parMapN(apply)
