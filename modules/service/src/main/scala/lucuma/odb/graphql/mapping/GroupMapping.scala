// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.Monad
import cats.effect.Resource
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.functor.*
import eu.timepit.refined.types.numeric.NonNegShort
import grackle.Query
import grackle.Query.Binding
import grackle.Query.EffectHandler
import grackle.Query.FilterOrderByOffsetLimit
import grackle.Query.OrderSelection
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import lucuma.core.model.Group
import lucuma.core.model.User
import lucuma.core.model.sequence.BandedTime
import lucuma.core.model.sequence.CategorizedTimeRange
import lucuma.core.util.CalculatedValue
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.GroupElementView
import lucuma.odb.graphql.table.GroupView
import lucuma.odb.graphql.table.ProgramTable
import lucuma.odb.json.calculatedValue.given
import lucuma.odb.json.time.query.given
import lucuma.odb.json.timeaccounting.given
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services

import Services.Syntax.*


trait GroupMapping[F[_]] extends GroupView[F] with ProgramTable[F] with GroupElementView[F] with KeyValueEffectHandler[F] with Predicates[F]:

  def user: User
  def itcClient: ItcClient[F]
  def services: Resource[F, Services[F]]
  def commitHash: CommitHash
  def timeEstimateCalculator: TimeEstimateCalculatorImplementation.ForInstrumentMode

  lazy val GroupMapping =
    ObjectMapping(GroupType)(
      SqlField("id", GroupView.Id, key = true),
      SqlField("parentId", GroupView.ParentId),
      SqlField("parentIndex", GroupView.ParentIndex),
      SqlField("name", GroupView.Name),
      SqlField("description", GroupView.Description),
      SqlField("minimumRequired", GroupView.MinRequired),
      SqlField("ordered", GroupView.Ordered),
      SqlObject("minimumInterval"),
      SqlObject("maximumInterval"),
      SqlObject("elements", Join(GroupView.Id, GroupElementView.GroupId)),
      SqlObject("program", Join(GroupView.ProgramId, ProgramTable.Id)),

      // OBSCALC TODO: replace with obscald version (2) below
      EffectField("timeEstimateRange", estimateRangeHandler, List("id")),
      EffectField("timeEstimateBanded", estimateBandedHandler, List("id")),
      EffectField("timeEstimateRange2", estimateRangeHandler2, List("id")),
      EffectField("timeEstimateBanded2", estimateBandedHandler2, List("id")),

      SqlField("existence", GroupView.Existence),
      SqlField("system", GroupView.System),
    )

  lazy val GroupElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (GroupType, "elements", List(
      BooleanBinding("includeDeleted", rIncludeDeleted)
    )) =>
      Elab.transformChild { child =>
        rIncludeDeleted.map: includeDeleted =>
          FilterOrderByOffsetLimit(
            pred = Some(Predicates.groupElement.existence.includeDeleted(includeDeleted)),
            oss = Some(List(OrderSelection[NonNegShort](GroupElementType / "parentIndex"))),
            offset = None,
            limit = None,
            child = child
          )
      }

  // OBSCALC TODO: Replace with obscalc version 2 below
  private lazy val estimateRangeHandler: EffectHandler[F] =
    keyValueEffectHandler[Group.Id, Option[CategorizedTimeRange]]("id"): gid =>
      services.useNonTransactionally:
        timeEstimateService(commitHash, itcClient, timeEstimateCalculator)
          .estimateGroupRange(gid)

  // OBSCALC TODO: Replace with obscalc version 2 below
  private lazy val estimateBandedHandler: EffectHandler[F] =
    keyValueEffectHandler[Group.Id, List[BandedTime]]("id"): gid =>
      services.useNonTransactionally:
        timeEstimateService(commitHash, itcClient, timeEstimateCalculator)
          .estimateGroupBanded(gid)
          .map(_.toList.flatMap(_.toList.sortBy(_._1).map((b, t) => BandedTime(b, t))))

  private lazy val estimateRangeHandler2: EffectHandler[F] =
    keyValueEffectHandler[Group.Id, Option[CalculatedValue[CategorizedTimeRange]]]("id"): gid =>
      services.useTransactionally:
        timeEstimateService2(commitHash, itcClient, timeEstimateCalculator)
          .estimateGroupRange(gid)

  private lazy val estimateBandedHandler2: EffectHandler[F] =
    keyValueEffectHandler[Group.Id, List[CalculatedValue[BandedTime]]]("id"): gid =>
      services.useTransactionally:
        timeEstimateService2(commitHash, itcClient, timeEstimateCalculator)
          .estimateGroupBanded(gid)
          .map(_.toList.sortBy(_._1).map((b, cv) => Monad[CalculatedValue].map(cv)(t => BandedTime(b, t))))