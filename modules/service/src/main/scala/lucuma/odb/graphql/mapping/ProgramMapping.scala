// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.Resource
import cats.syntax.all._
import eu.timepit.refined.types.numeric.NonNegShort
import grackle.Cursor
import grackle.Predicate
import grackle.Predicate._
import grackle.Query
import grackle.Query._
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.ResultT
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import io.circe.syntax.*
import lucuma.core.model.Group
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.CategorizedTimeRange
import lucuma.itc.client.ItcClient
import lucuma.odb.data.Tag
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.GroupElementView
import lucuma.odb.json.time.query.given
import lucuma.odb.json.timeaccounting.given
import lucuma.odb.logic.TimeEstimateCalculator
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
     with GroupElementView[F]
     with UserInvitationTable[F] {

  def user: User
  def itcClient: ItcClient[F]
  def services: Resource[F, Services[F]]
  def commitHash: CommitHash
  def timeEstimateCalculator: TimeEstimateCalculator.ForInstrumentMode

  lazy val ProgramMapping: ObjectMapping =
    ObjectMapping(
      tpe = ProgramType,
      fieldMappings = List(
        SqlField("id", ProgramTable.Id, key = true),
        SqlField("existence", ProgramTable.Existence, hidden = true),
        SqlField("name", ProgramTable.Name),
        SqlField("semester", ProgramTable.Reference.Semester),
        SqlField("semesterIndex", ProgramTable.Reference.SemesterIndex),
        SqlField("reference", ProgramTable.Reference.ProgramReference),
        SqlField("piUserId", ProgramTable.PiUserId, hidden = true),
        SqlField("proposalStatus", ProgramTable.ProposalStatus),
        SqlObject("pi", Join(ProgramTable.PiUserId, UserTable.UserId)),
        SqlObject("users", Join(ProgramTable.Id, ProgramUserTable.ProgramId)),
        SqlObject("observations"),
        SqlObject("proposal", Join(ProgramTable.Id, ProposalTable.ProgramId)),
        SqlObject("groupElements", Join(ProgramTable.Id, GroupElementView.ProgramId)),
        SqlObject("allGroupElements", Join(ProgramTable.Id, GroupElementView.ProgramId)),
        SqlObject("obsAttachments", Join(ProgramTable.Id, ObsAttachmentTable.ProgramId)),
        SqlObject("proposalAttachments", Join(ProgramTable.Id, ProposalAttachmentTable.ProgramId)),
        EffectField("timeEstimateRange", timeEstimateHandler, List("id")),
        EffectField("timeCharge", timeChargeHandler, List("id")),
        SqlObject("userInvitations", Join(ProgramTable.Id, UserInvitationTable.ProgramId)),
      )
    )

  lazy val ProgramElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {

    case (ProgramType, "observations", List(
      BooleanBinding("includeDeleted", rIncludeDeleted),
      ObservationIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT),
    )) =>
      Elab.transformChild { child =>
        (rIncludeDeleted, rOFFSET, rLIMIT).parTupled.flatMap { (includeDeleted, OFFSET, lim) =>
          val limit = lim.fold(ResultMapping.MaxLimit)(_.value)
          ResultMapping.selectResult(child, limit) { q =>
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
      }

    case (ProgramType, "groupElements", Nil) =>
      Elab.transformChild { child =>
        FilterOrderByOffsetLimit(
          pred = Some(Predicates.groupElement.parentGroupId.isNull(true)),
          oss = Some(List(OrderSelection[NonNegShort](GroupElementType / "parentIndex", true, true))),
          offset = None,
          limit = None,
          child
        )
      }

    case (ProgramType, "allGroupElements", Nil) =>
      Elab.transformChild { child =>
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
      }

    case (ProgramType, "obsAttachments", Nil) =>
      Elab.transformChild { child =>
        OrderBy(OrderSelections(List(OrderSelection[ObsAttachment.Id](ObsAttachmentType / "id"))), child)
      }

    case (ProgramType, "proposalAttachments", Nil) =>
      Elab.transformChild { child =>
        OrderBy(OrderSelections(List(OrderSelection[Tag](ProposalAttachmentType / "attachmentType"))), child)
      }
  }

  private abstract class ProgramEffectHandler[T: io.circe.Encoder] extends EffectHandler[F] {
    def calculate(pid: Program.Id): F[T]

    override def runEffects(queries: List[(Query, Cursor)]): F[Result[List[Cursor]]] =
      (for {
        ctx <- ResultT(queries.traverse { case (_, cursor) => cursor.fieldAs[Program.Id]("id") }.pure[F])
        prg <- ctx.distinct.traverse { pid => ResultT(calculate(pid).map(Result.success)).tupleLeft(pid) }
        res <- ResultT(ctx
                 .flatMap(pid => prg.find(r => r._1 === pid).map(_._2).toList)
                 .zip(queries)
                 .traverse { case (result, (query, parentCursor)) =>
                   Query.childContext(parentCursor.context, query).map { childContext =>
                     CirceCursor(childContext, result.asJson, Some(parentCursor), parentCursor.fullEnv)
                   }
                 }.pure[F]
               )
        } yield res
      ).value
    }

  private lazy val timeEstimateHandler: EffectHandler[F] =
    new ProgramEffectHandler[Option[CategorizedTimeRange]] {
      override def calculate(pid: Program.Id): F[Option[CategorizedTimeRange]] =
        services.useNonTransactionally {
          timeEstimateService(commitHash, itcClient, timeEstimateCalculator)
            .estimateProgram(pid)
        }
    }

  private val timeChargeHandler: EffectHandler[F] =
    new ProgramEffectHandler[CategorizedTime] {
      def calculate(pid: Program.Id): F[CategorizedTime] =
        services.useTransactionally {
          timeAccountingService.selectProgram(pid)
        }
    }
}