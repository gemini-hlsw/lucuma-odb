// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.option.*
import eu.timepit.refined.cats.*
import grackle.Cursor
import grackle.Predicate
import grackle.Predicate.Const
import grackle.Predicate.Eql
import grackle.Query.Binding
import grackle.Query.OrderBy
import grackle.Query.OrderSelection
import grackle.Query.OrderSelections
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.Type
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import lucuma.core.enums.ScienceSubtype
import lucuma.core.model.IntPercent
import lucuma.odb.data.Tag
import lucuma.odb.graphql.table.CallForProposalsView
import lucuma.odb.graphql.table.PartnerSplitTable
import lucuma.odb.graphql.table.ProposalView

trait CallPropertiesMapping[F[_]] extends BaseMapping[F]
                                     with CallForProposalsView[F]
                                     with PartnerSplitTable[F]
                                     with ProposalView[F] {

  lazy val CallPropertiesMapping: ObjectMapping =
    SqlInterfaceMapping(
      tpe = CallPropertiesType,
      discriminator = callPropertiesTypeDiscriminator,
      fieldMappings = List(
        SqlField("id", ProposalView.ProgramId, key = true, hidden = true),
        SqlField("scienceSubtype", ProposalView.ScienceSubtype, discriminator = true),
        SqlObject("call", Join(ProposalView.CallId, CallForProposalsView.Id))
      )
    )

  private lazy val callPropertiesTypeDiscriminator: SqlDiscriminator =
    new SqlDiscriminator {
      override def discriminate(c:  Cursor): Result[Type] =
        c.fieldAs[ScienceSubtype]("scienceSubtype").flatMap {
          case ScienceSubtype.Classical          => Result(CallPropertiesClassicalType)
          case ScienceSubtype.DemoScience        => Result(CallPropertiesDemoScienceType)
          case ScienceSubtype.DirectorsTime      => Result(CallPropertiesDirectorsTimeType)
          case ScienceSubtype.FastTurnaround     => Result(CallPropertiesFastTurnaroundType)
          case ScienceSubtype.LargeProgram       => Result(CallPropertiesLargeProgramType)
          case ScienceSubtype.PoorWeather        => Result(CallPropertiesPoorWeatherType)
          case ScienceSubtype.Queue              => Result(CallPropertiesQueueType)
          case ScienceSubtype.SystemVerification => Result(CallPropertiesSystemVerificationType)
        }

      private def mkPredicate(tpe: ScienceSubtype): Option[Predicate] =
        Eql(CallPropertiesType / "scienceSubtype", Const(tpe)).some

      override def narrowPredicate(tpe:  Type): Option[Predicate] =
        tpe match {
          case CallPropertiesClassicalType          => mkPredicate(ScienceSubtype.Classical)
          case CallPropertiesDemoScienceType        => mkPredicate(ScienceSubtype.DemoScience)
          case CallPropertiesDirectorsTimeType      => mkPredicate(ScienceSubtype.DirectorsTime)
          case CallPropertiesFastTurnaroundType     => mkPredicate(ScienceSubtype.FastTurnaround)
          case CallPropertiesLargeProgramType       => mkPredicate(ScienceSubtype.LargeProgram)
          case CallPropertiesPoorWeatherType        => mkPredicate(ScienceSubtype.PoorWeather)
          case CallPropertiesQueueType              => mkPredicate(ScienceSubtype.Queue)
          case CallPropertiesSystemVerificationType => mkPredicate(ScienceSubtype.SystemVerification)
          case _                                    => none
        }
    }

  lazy val CallPropertiesClassicalMapping: ObjectMapping =
    ObjectMapping(CallPropertiesClassicalType)(
      SqlField("id", ProposalView.Classical.Id, key = true, hidden = true),
      SqlField("scienceSubtype",  ProposalView.ScienceSubtype),
      SqlObject("call", Join(ProposalView.CallId, CallForProposalsView.Id)),
      SqlField("minPercentTime",  ProposalView.MinPercent),
      SqlObject("partnerSplits",  Join(ProposalView.Classical.Id, PartnerSplitTable.ProgramId))
    )

  lazy val CallPropertiesDemoScienceMapping: ObjectMapping =
    ObjectMapping(CallPropertiesDemoScienceType)(
      SqlField("id", ProposalView.DemoScience.Id, key = true, hidden = true),
      SqlField("scienceSubtype",  ProposalView.ScienceSubtype),
      SqlObject("call", Join(ProposalView.CallId, CallForProposalsView.Id)),
      SqlField("toOActivation",   ProposalView.TooActivation),
      SqlField("minPercentTime",  ProposalView.MinPercent)
    )

  lazy val CallPropertiesDirectorsTimeMapping: ObjectMapping =
    ObjectMapping(CallPropertiesDirectorsTimeType)(
      SqlField("id", ProposalView.DirectorsTime.Id, key = true, hidden = true),
      SqlField("scienceSubtype",  ProposalView.ScienceSubtype),
      SqlObject("call", Join(ProposalView.CallId, CallForProposalsView.Id)),
      SqlField("toOActivation",   ProposalView.TooActivation),
      SqlField("minPercentTime",  ProposalView.MinPercent)
    )

  lazy val CallPropertiesFastTurnaroundMapping: ObjectMapping =
    ObjectMapping(CallPropertiesFastTurnaroundType)(
      SqlField("id", ProposalView.FastTurnaround.Id, key = true, hidden = true),
      SqlField("scienceSubtype",  ProposalView.ScienceSubtype),
      SqlObject("call", Join(ProposalView.CallId, CallForProposalsView.Id)),
      SqlField("toOActivation",   ProposalView.TooActivation),
      SqlField("minPercentTime",  ProposalView.MinPercent),
      SqlField("piAffiliation",   ProposalView.FastTurnaround.PiAffiliate)
    )

  lazy val CallPropertiesLargeProgramMapping: ObjectMapping =
    ObjectMapping(CallPropertiesLargeProgramType)(
      SqlField("id", ProposalView.LargeProgram.Id, key = true, hidden = true),
      SqlField("scienceSubtype",      ProposalView.ScienceSubtype),
      SqlObject("call", Join(ProposalView.CallId, CallForProposalsView.Id)),
      SqlField("toOActivation",       ProposalView.TooActivation),
      SqlField("minPercentTime",      ProposalView.MinPercent),
      SqlField("minPercentTotalTime", ProposalView.LargeProgram.MinPercentTotal),
      SqlObject("totalTime")
    )

  lazy val CallPropertiesPoorWeatherMapping: ObjectMapping =
    ObjectMapping(CallPropertiesPoorWeatherType)(
      SqlField("id", ProposalView.PoorWeather.Id, key = true, hidden = true),
      SqlField("scienceSubtype", ProposalView.ScienceSubtype),
      SqlObject("call", Join(ProposalView.CallId, CallForProposalsView.Id))
    )

  lazy val CallPropertiesQueueMapping: ObjectMapping =
    ObjectMapping(CallPropertiesQueueType)(
      SqlField("id", ProposalView.Queue.Id, key = true, hidden = true),
      SqlField("scienceSubtype",  ProposalView.ScienceSubtype),
      SqlObject("call", Join(ProposalView.CallId, CallForProposalsView.Id)),
      SqlField("toOActivation",   ProposalView.TooActivation),
      SqlField("minPercentTime",  ProposalView.MinPercent),
      SqlObject("partnerSplits",  Join(ProposalView.Queue.Id, PartnerSplitTable.ProgramId))
    )

  lazy val CallPropertiesSystemVerificationMapping: ObjectMapping =
    ObjectMapping(CallPropertiesSystemVerificationType)(
      SqlField("id", ProposalView.SystemVerification.Id, key = true, hidden = true),
      SqlField("scienceSubtype",  ProposalView.ScienceSubtype),
      SqlObject("call", Join(ProposalView.CallId, CallForProposalsView.Id)),
      SqlField("toOActivation",   ProposalView.TooActivation),
      SqlField("minPercentTime",  ProposalView.MinPercent)
    )

  val SortSplits: Elab[Unit] =
    Elab.transformChild { child =>
      OrderBy(
        OrderSelections(
          List(
            OrderSelection[IntPercent](PartnerSplitType / "percent", ascending = false),
            OrderSelection[Tag](PartnerSplitType / "partner")
          )
        ),
        child
      )
    }

  lazy val CallPropertiesElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    case (CallPropertiesClassicalType, "partnerSplits", Nil) => SortSplits
    case (CallPropertiesQueueType,     "partnerSplits", Nil) => SortSplits
  }

}