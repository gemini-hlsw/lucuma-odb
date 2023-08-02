// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.all._
import cats.effect.Resource
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.ResultT
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.Group
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.sequence.PlannedTime
import lucuma.core.model.sequence.PlannedTimeRange
import lucuma.itc.client.ItcClient
import lucuma.odb.data.GroupTree
import lucuma.odb.data.Tag
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.GroupElementView
import lucuma.odb.logic.PlannedTimeCalculator
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services

import Services.Syntax.*

import binding._
import table._

trait ProgramMapping[F[_]]
  extends ProgramTable[F]
     with UserTable[F]
     with ProgramUserTable[F]
     with ProposalTable[F]
     with ObservationView[F]
     with ObsAttachmentTable[F]
     with Predicates[F]
     with ProposalAttachmentTable[F]
     with ResultMapping[F]
     with GroupElementView[F] {

  def user: User
  def itcClient: ItcClient[F]
  def services: Resource[F, Services[F]]
  def commitHash: CommitHash
  def plannedTimeCalculator: PlannedTimeCalculator.ForInstrumentMode

  lazy val ProgramMapping: ObjectMapping =
    ObjectMapping(
      tpe = ProgramType,
      fieldMappings = List(
        SqlField("id", ProgramTable.Id, key = true),
        SqlField("existence", ProgramTable.Existence, hidden = true),
        SqlField("name", ProgramTable.Name),
        SqlField("piUserId", ProgramTable.PiUserId, hidden = true),
        SqlObject("pi", Join(ProgramTable.PiUserId, UserTable.UserId)),
        SqlObject("users", Join(ProgramTable.Id, ProgramUserTable.ProgramId)),
        SqlObject("observations"),
        SqlObject("proposal", Join(ProgramTable.Id, ProposalTable.ProgramId)),
        SqlObject("groupElements", Join(ProgramTable.Id, GroupElementView.ProgramId)),
        SqlObject("allGroupElements", Join(ProgramTable.Id, GroupElementView.ProgramId)),
        SqlObject("obsAttachments", Join(ProgramTable.Id, ObsAttachmentTable.ProgramId)),
        SqlObject("proposalAttachments", Join(ProgramTable.Id, ProposalAttachmentTable.ProgramId)),
        EffectField("plannedTimeRange", plannedTimeHandler, List("id"))
      )
    )

  lazy val ProgramElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    Map(
      ProgramType -> {
        case Select("observations", List(
          BooleanBinding("includeDeleted", rIncludeDeleted),
          ObservationIdBinding.Option("OFFSET", rOFFSET),
          NonNegIntBinding.Option("LIMIT", rLIMIT),
        ), child) =>
          (rIncludeDeleted, rOFFSET, rLIMIT).parTupled.flatMap { (includeDeleted, OFFSET, lim) =>
            val limit = lim.fold(ResultMapping.MaxLimit)(_.value)
            ResultMapping.selectResult("observations", child, limit) { q =>
              FilterOrderByOffsetLimit(
                pred = Some(and(List(
                  Predicates.observation.existence.includeDeleted(includeDeleted),
                  OFFSET.fold[Predicate](True)(Predicates.observation.id.gtEql)
                ))),
                oss = Some(List(OrderSelection[Observation.Id](ObservationType / "id", true, true))),
                offset = None,
                limit = Some(limit + 1),
                q
              )
            }
          }
        case Select("groupElements", Nil, child) =>
          Result(
            Select("groupElements", Nil,
              FilterOrderByOffsetLimit(
                pred = Some(Predicates.groupElement.parentGroupId.isNull(true)),
                oss = Some(List(OrderSelection[NonNegShort](GroupElementType / "parentIndex", true, true))),
                offset = None,
                limit = None,
                child
              )
            )
          )
        case Select("allGroupElements", Nil, child) =>
          Result(
            Select("allGroupElements", Nil,
              FilterOrderByOffsetLimit(
                pred = None,
                oss = Some(List(
                  OrderSelection[Option[Group.Id]](GroupElementType / "parentGroupId", true, true),
                  OrderSelection[NonNegShort](GroupElementType / "parentIndex", true, true)
                )),
                offset = None,
                limit = None,
                child
              )
            )
          )
        case Select("obsAttachments", Nil, child) =>
          Result(
            Select("obsAttachments", Nil,
              OrderBy(OrderSelections(List(OrderSelection[ObsAttachment.Id](ObsAttachmentType / "id"))), child)
            )
          )
        case Select("proposalAttachments", Nil, child) =>
          Result(
            Select("proposalAttachments", Nil,
              OrderBy(OrderSelections(List(OrderSelection[Tag](ProposalAttachmentType / "attachmentType"))), child)
            )
          )
      }
    )

  lazy val plannedTimeHandler: EffectHandler[F] =
    new EffectHandler[F] {
      val longestToShortest = catsKernelOrderingForOrder(Order.reverse(Order[PlannedTime]))

      def combine(minRequired: Int, children: List[PlannedTimeRange]): Option[PlannedTimeRange] =
        Option.when(children.size >= minRequired) {
          PlannedTimeRange.from(
            children.map(_.min).sorted.take(minRequired).combineAllOption.getOrElse(PlannedTime.Zero),
            children.map(_.max).sorted(longestToShortest).take(minRequired).combineAllOption.getOrElse(PlannedTime.Zero)
          )
        }

      def leafRange(pid: Program.Id, oid: Observation.Id): F[Option[PlannedTimeRange]] =
        services.useNonTransactionally {
          generator(commitHash, itcClient, plannedTimeCalculator)
            .digest(pid, oid)
            .map(_.toOption.map(d => PlannedTimeRange.single(d.fullPlannedTime)))
        }

      def parentRange(pid: Program.Id, minRequired: Option[NonNegShort], children: List[GroupTree.Child]): F[Option[PlannedTimeRange]] =
        children
          .traverse(plannedTimeRange(pid, _))
          .map(lst => combine(minRequired.fold(lst.size)(_.value.toInt), lst.flatMap(_.toList)))

      def plannedTimeRange(pid: Program.Id, root: GroupTree): F[Option[PlannedTimeRange]] =
        root match {
          case GroupTree.Leaf(oid)                               => leafRange(pid, oid)
          case GroupTree.Branch(_, min, _, children, _, _, _, _) => parentRange(pid, min, children)
          case GroupTree.Root(_, children)                       => parentRange(pid, None, children)
        }

      def calculate(pid: Program.Id): F[Option[PlannedTimeRange]] =
        services
          .useTransactionally(groupService.selectGroups(pid))
          .flatMap(plannedTimeRange(pid, _))

      def runEffects(queries: List[(Query, Cursor)]): F[Result[List[(Query, Cursor)]]] =
        (for {
          ctx <- ResultT(queries.traverse { case (_, cursor) => cursor.fieldAs[Program.Id]("id") }.pure[F])
          res <- ctx.distinct.traverse { pid => ResultT(calculate(pid).map(Result.success)).tupleLeft(pid) }
        } yield
          ctx
            .flatMap(pid => res.find(r => r._1 === pid).map(_._2).toList)
            .zip(queries)
            .map { case (result, (child, parentCursor)) =>
              import lucuma.odb.json.plannedtime.given
              import lucuma.odb.json.time.query.given
              val json: Json     = Json.fromFields(List("plannedTimeRange" -> result.asJson))
              val cursor: Cursor = CirceCursor(parentCursor.context, json, Some(parentCursor), parentCursor.fullEnv)
              (child, cursor)
            }
        ).value
    }

}

