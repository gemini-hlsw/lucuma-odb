// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

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
import grackle.syntax.*
import lucuma.core.enums.Partner
import lucuma.core.enums.ScienceSubtype
import lucuma.core.model.IntPercent
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.PartnerSplitTable
import lucuma.odb.graphql.table.ProgramUserView
import lucuma.odb.graphql.table.ProposalView

trait ProposalTypeMapping[F[_]] extends BaseMapping[F]
                                   with Predicates[F]
                                   with PartnerSplitTable[F]
                                   with ProgramUserView[F]
                                   with ProposalView[F] {

  lazy val ProposalTypeMapping: ObjectMapping =
    SqlInterfaceMapping(
      tpe = ProposalTypeType,
      discriminator = proposalTypeDiscriminator,
      fieldMappings = List(
        SqlField("id", ProposalView.ProgramId, key = true, hidden = true),
        SqlField("scienceSubtype", ProposalView.ScienceSubtype, discriminator = true),
      )
    )

  private lazy val proposalTypeDiscriminator: SqlDiscriminator =
    new SqlDiscriminator {
      override def discriminate(c:  Cursor): Result[Type] =
        c.fieldAs[ScienceSubtype]("scienceSubtype").flatMap:
          case ScienceSubtype.Classical          => Result(ClassicalType)
          case ScienceSubtype.DemoScience        => Result(DemoScienceType)
          case ScienceSubtype.DirectorsTime      => Result(DirectorsTimeType)
          case ScienceSubtype.FastTurnaround     => Result(FastTurnaroundType)
          case ScienceSubtype.LargeProgram       => Result(LargeProgramType)
          case ScienceSubtype.PoorWeather        => Result(PoorWeatherType)
          case ScienceSubtype.Queue              => Result(QueueType)
          case ScienceSubtype.SystemVerification => Result(SystemVerificationType)

      private def mkPredicate(tpe: ScienceSubtype): Result[Predicate] =
        Eql(ProposalTypeType / "scienceSubtype", Const(tpe)).success

      override def narrowPredicate(tpe:  Type): Result[Predicate] =
        tpe match
          case ClassicalType          => mkPredicate(ScienceSubtype.Classical)
          case DemoScienceType        => mkPredicate(ScienceSubtype.DemoScience)
          case DirectorsTimeType      => mkPredicate(ScienceSubtype.DirectorsTime)
          case FastTurnaroundType     => mkPredicate(ScienceSubtype.FastTurnaround)
          case LargeProgramType       => mkPredicate(ScienceSubtype.LargeProgram)
          case PoorWeatherType        => mkPredicate(ScienceSubtype.PoorWeather)
          case QueueType              => mkPredicate(ScienceSubtype.Queue)
          case SystemVerificationType => mkPredicate(ScienceSubtype.SystemVerification)
          case _                      => Result.internalError(s"Invalid discriminator: $tpe")
    }

  lazy val ClassicalMapping: ObjectMapping =
    ObjectMapping(ClassicalType)(
      SqlField("id", ProposalView.Classical.Id, key = true, hidden = true),
      SqlField("minPercentTime",  ProposalView.MinPercent),
      SqlObject("partnerSplits",  Join(ProposalView.Classical.Id, PartnerSplitTable.ProgramId))
    )

  lazy val DemoScienceMapping: ObjectMapping =
    ObjectMapping(DemoScienceType)(
      SqlField("id", ProposalView.DemoScience.Id, key = true, hidden = true),
      SqlField("toOActivation",   ProposalView.TooActivation),
      SqlField("minPercentTime",  ProposalView.MinPercent)
    )

  lazy val DirectorsTimeMapping: ObjectMapping =
    ObjectMapping(DirectorsTimeType)(
      SqlField("id", ProposalView.DirectorsTime.Id, key = true, hidden = true),
      SqlField("toOActivation",   ProposalView.TooActivation),
      SqlField("minPercentTime",  ProposalView.MinPercent)
    )

  lazy val FastTurnaroundMapping: ObjectMapping =
    ObjectMapping(FastTurnaroundType)(
      SqlField("id", ProposalView.FastTurnaround.Id, key = true, hidden = true),
      SqlField("toOActivation",   ProposalView.TooActivation),
      SqlField("minPercentTime",  ProposalView.MinPercent),
      SqlObject("reviewer",       Join(ProposalView.FastTurnaround.ReviewerId, ProgramUserView.ProgramUserId)),
      SqlObject("mentor",         Join(ProposalView.FastTurnaround.MentorId, ProgramUserView.ProgramUserId))
    )

  lazy val LargeProgramMapping: ObjectMapping =
    ObjectMapping(LargeProgramType)(
      SqlField("id", ProposalView.LargeProgram.Id, key = true, hidden = true),
      SqlField("toOActivation",       ProposalView.TooActivation),
      SqlField("minPercentTime",      ProposalView.MinPercent),
      SqlField("minPercentTotalTime", ProposalView.LargeProgram.MinPercentTotal),
      SqlObject("totalTime")
    )

  lazy val PoorWeatherMapping: ObjectMapping =
    ObjectMapping(PoorWeatherType)(
      SqlField("id", ProposalView.PoorWeather.Id, key = true, hidden = true),
    )

  lazy val QueueMapping: ObjectMapping =
    ObjectMapping(QueueType)(
      SqlField("id", ProposalView.Queue.Id, key = true, hidden = true),
      SqlField("toOActivation",   ProposalView.TooActivation),
      SqlField("minPercentTime",  ProposalView.MinPercent),
      SqlObject("partnerSplits",  Join(ProposalView.Queue.Id, PartnerSplitTable.ProgramId))
    )

  lazy val SystemVerificationMapping: ObjectMapping =
    ObjectMapping(SystemVerificationType)(
      SqlField("id", ProposalView.SystemVerification.Id, key = true, hidden = true),
      SqlField("toOActivation",   ProposalView.TooActivation),
      SqlField("minPercentTime",  ProposalView.MinPercent)
    )

  val SortSplits: Elab[Unit] =
    Elab.transformChild { child =>
      OrderBy(
        OrderSelections(
          List(
            OrderSelection[IntPercent](PartnerSplitType / "percent", ascending = false),
            OrderSelection[Partner](PartnerSplitType / "partner")
          )
        ),
        child
      )
    }

  lazy val ProposalTypeElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    case (ClassicalType, "partnerSplits", Nil) => SortSplits
    case (QueueType,     "partnerSplits", Nil) => SortSplits
  }

}
