// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.Resource
import cats.syntax.all._
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.QueryCompiler.Elab
import edu.gemini.grackle.Result
import edu.gemini.grackle.ResultT
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.syntax.*
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.Group
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.sequence.PlannedTimeRange
import lucuma.itc.client.ItcClient
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

  lazy val plannedTimeHandler: EffectHandler[F] =
    new EffectHandler[F] {

      def calculate(pid: Program.Id): F[Option[PlannedTimeRange]] =
        services.useNonTransactionally {
          plannedTimeRangeService(commitHash, itcClient, plannedTimeCalculator)
            .estimateProgram(pid)
        }

      def runEffects(queries: List[(Query, Cursor)]): F[Result[List[(Query, Cursor)]]] =
        (for {
          ctx <- ResultT(queries.traverse { case (_, cursor) => cursor.fieldAs[Program.Id]("id") }.pure[F])
          ps  <- ctx.distinct.traverse { pid => ResultT(calculate(pid).map(Result.success)).tupleLeft(pid) }
          res <- ResultT(ctx
                    .flatMap(pid => ps.find(r => r._1 === pid).map(_._2).toList)
                    .zip(queries)
                    .traverse { case (result, (child, childCursor)) =>
                      import lucuma.odb.json.plannedtime.given
                      import lucuma.odb.json.time.query.given
                      childCursor.context.parent.toResultOrError("No parent context").map { parentContext =>
                        val parentField    = childCursor.path.head
                        val json: Json     = Json.fromFields(List(parentField -> Json.fromFields(List("plannedTimeRange" -> result.asJson))))
                        val cursor: Cursor = CirceCursor(parentContext, json, Some(childCursor), childCursor.fullEnv)
                        (Query.Select(parentField, None, child), cursor)
                      }
                    }.pure[F]
                  )
          } yield res
        ).value
        
  }
}

