// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.Resource
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
import lucuma.core.model.sequence.CategorizedTimeRange
import lucuma.itc.client.ItcClient
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.GroupElementView
import lucuma.odb.graphql.table.GroupView
import lucuma.odb.graphql.table.ProgramTable
import lucuma.odb.json.time.query.given
import lucuma.odb.json.timeaccounting.given
import lucuma.odb.logic.TimeEstimateCalculator
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services

import Services.Syntax.*


trait GroupMapping[F[_]] extends GroupView[F] with ProgramTable[F] with GroupElementView[F] with KeyValueEffectHandler[F] with Predicates[F] {

  def user: User
  def itcClient: ItcClient[F]
  def services: Resource[F, Services[F]]
  def commitHash: CommitHash
  def timeEstimateCalculator: TimeEstimateCalculator.ForInstrumentMode

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
      EffectField("timeEstimateRange", timeEstimateHandler, List("id")),
      SqlField("existence", GroupView.Existence),
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

  private lazy val timeEstimateHandler: EffectHandler[F] =
    keyValueEffectHandler[Group.Id, Option[CategorizedTimeRange]]("id") { gid =>
      services.useNonTransactionally {
        timeEstimateService(commitHash, itcClient, timeEstimateCalculator)
          .estimateGroup(gid)
      }
    }

}

