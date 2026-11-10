// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import lucuma.core.enums.ScienceBand

import table.TacFeedbackTable

trait TacFeedbackMapping[F[_]] extends TacFeedbackTable[F]:

  lazy val TacFeedbackMapping =
    ObjectMapping(TacFeedbackType)(
      SqlField("programId", TacFeedbackTable.ProgramId, key = true, hidden = true),
      SqlField("timeAccountingCategory", TacFeedbackTable.Category, key = true),
      SqlField("decision",  TacFeedbackTable.Decision),
      SqlField("ranking",   TacFeedbackTable.Ranking),
      SqlObject("recommendedTimes", Join(List(
        TacFeedbackTable.ProgramId -> TacRecommendedTimeTable.ProgramId,
        TacFeedbackTable.Category  -> TacRecommendedTimeTable.Category
      ))),
      SqlField("comments",  TacFeedbackTable.Comments)
    )

  lazy val TacRecommendedTimeMapping =
    ObjectMapping(TacRecommendedTimeType)(
      SqlField("programId", TacRecommendedTimeTable.ProgramId,   key = true, hidden = true),
      SqlField("category",  TacRecommendedTimeTable.Category,    key = true, hidden = true),
      SqlField("band",      TacRecommendedTimeTable.ScienceBand, key = true),
      SqlObject("time")
    )

  lazy val TacFeedbackElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (TacFeedbackType, "recommendedTimes", Nil) =>
      Elab.transformChild: child =>
        OrderBy(OrderSelections(List(OrderSelection[ScienceBand](TacRecommendedTimeType / "band"))), child)
