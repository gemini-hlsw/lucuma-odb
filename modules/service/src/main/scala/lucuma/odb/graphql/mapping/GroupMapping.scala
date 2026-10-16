// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.Monad
import cats.effect.Resource
import cats.implicits.catsKernelOrderingForOrder
import cats.syntax.functor.*
import eu.timepit.refined.types.numeric.NonNegShort
import grackle.Query.Binding
import grackle.Query.EffectHandler
import grackle.Query.FilterOrderByOffsetLimit
import grackle.Query.OrderSelection
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import grackle.syntax.*
import io.circe.syntax.*
import lucuma.core.model.Group
import lucuma.core.model.User
import lucuma.core.model.sequence.BandedTime
import lucuma.core.util.CalculatedValue
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.GroupElementView
import lucuma.odb.graphql.table.GroupView
import lucuma.odb.graphql.table.ProgramView
import lucuma.odb.json.calculatedValue.given
import lucuma.odb.json.time.query.given
import lucuma.odb.json.timeaccounting.given
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.logic.TimeEstimateService
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services
import org.http4s.client.Client

import Services.Syntax.*


trait GroupMapping[F[_]] extends GroupView[F] with ProgramView[F] with GroupElementView[F] with KeyValueEffectHandler[F] with Predicates[F]:

  def itcClient: ItcClient[F]
  def services(using User): Resource[F, Services[F]]
  def commitHash: CommitHash
  def timeEstimateCalculator: TimeEstimateCalculatorImplementation.ForInstrumentMode
  def emailConfig: Config.Email
  def httpClient: Client[F]

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
      SqlField("sameNight", GroupView.SameNight),
      SqlObject("elements", Join(GroupView.Id, GroupElementView.GroupId)),
      SqlObject("program", Join(GroupView.ProgramId, ProgramView.Id)),

      EffectField(RangeField, estimateHandler, List("id")),
      EffectField(BandedField, estimateHandler, List("id")),

      SqlField("existence", GroupView.Existence),
      SqlField("system", GroupView.System),
      SqlField("calibrationRoles", GroupView.CalibrationRoles),
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

  private val RangeField  = "timeEstimateRange"
  private val BandedField = "timeEstimateBanded"

  // One handler serves both estimate fields.  Grackle batches by handler, so it
  // receives every group for both fields at once and each program is loaded
  // only once.
  private lazy val estimateHandler: EffectHandler[F] =
    keyedBatchEffectHandler[Group.Id, TimeEstimateService.GroupEstimate](
      _.fieldAs[Group.Id]("id"),
      (gids, fields) =>
        services.useTransactionally:
          timeEstimateService
            .estimateGroups(gids, fields.contains(BandedField))
            .map(_.view.mapValues(_.success).toMap),
      {
        case (RangeField, e)  =>
          e.range.asJson.success
        case (BandedField, e) =>
          e.banded.toList.sortBy(_._1).map((b, cv) => Monad[CalculatedValue].map(cv)(t => BandedTime(b, t))).asJson.success
        case (field, _)       =>
          Result.internalError(s"Unexpected group time estimate field: $field")
      }
    )
