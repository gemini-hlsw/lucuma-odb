// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.Resource
import cats.syntax.all.*
import grackle.Cursor
import grackle.Cursor.ListTransformCursor
import grackle.Env
import grackle.Query
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.ResultT
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import grackle.syntax.*
import io.circe.Json
import lucuma.core.model.Attachment
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.itc.client.ItcClient
import lucuma.odb.data.Itc
import lucuma.odb.data.ItcAcquisition
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.TimingWindowView
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services
import skunk.Transaction

import table.AttachmentTable
import table.ArchiveDuplicationView
import table.ObsAttachmentAssignmentTable
import table.ObscalcTable
import table.ObservationReferenceView
import table.ProgramView
import Services.Syntax.*

trait ObservationMapping[F[_]]
  extends ObservationEffectHandler[F]
     with Predicates[F]
     with ProgramView[F]
     with TimingWindowView[F]
     with AttachmentTable[F]
     with ObsAttachmentAssignmentTable[F]
     with ArchiveDuplicationView[F]
     with ObscalcTable[F]
     with ObservationReferenceView[F] {

  def itcClient: ItcClient[F]
  def services(using User): Resource[F, Services[F]]
  def commitHash: CommitHash
  def timeEstimateCalculator: TimeEstimateCalculatorImplementation.ForInstrumentMode

  lazy val ObservationMapping: ObjectMapping =
    ObjectMapping(ObservationType)(
      SqlField("id", ObservationView.Id, key = true),
      SqlField("programId", ObservationView.ProgramId, hidden = true),
      SqlField("existence", ObservationView.Existence),
      SqlObject("reference", Join(ObservationView.Id, ObservationReferenceView.Id)),
      SqlField("index", ObservationView.ObservationIndex),
      SqlField("title", ObservationView.Title),
      SqlField("subtitle", ObservationView.Subtitle),
      SqlField("scienceBand", ObservationView.ScienceBand),
      SqlField("observationTime", ObservationView.ObservationTime),
      SqlObject("observationDuration"),
      SqlObject("posAngleConstraint"),
      SqlObject("targetEnvironment"),
      SqlObject("constraintSet"),
      SqlObject("timingWindows", Join(ObservationView.Id, TimingWindowView.ObservationId)),
      SqlObject("schedulingConstraints"),
      SqlObject("attachments",
        Join(ObservationView.Id, ObsAttachmentAssignmentTable.ObservationId),
        Join(ObsAttachmentAssignmentTable.AttachmentId, AttachmentTable.Id)),
      SqlObject("scienceRequirements"),
      SqlObject("observingMode"),
      SqlField("instrument", ObservationView.Instrument),
      SqlObject("program", Join(ObservationView.ProgramId, ProgramView.Id)),
      EffectField("itc", itcQueryHandler, List("id", "programId")),
      SqlObject("execution"),
      SqlField("groupId", ObservationView.GroupId),
      SqlField("groupIndex", ObservationView.GroupIndex),
      SqlField("calibrationRole", ObservationView.CalibrationRole),
      SqlField("observerNotes", ObservationView.ObserverNotes),
      SqlField("priority", ObservationView.Priority),
      SqlObject("configuration"),
      EffectField("configurationRequests", configurationRequestsQueryHandler, List("id", "programId")),
      SqlObject("workflow", Join(ObservationView.Id, ObscalcTable.ObservationId)),
      SqlObject("archiveDuplication", Join(ObservationView.Id, ArchiveDuplicationView.ObservationId))
    )

  lazy val ObservationElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {

    case (ObservationType, "timingWindows", Nil) =>
      Elab.transformChild { child =>
        FilterOrderByOffsetLimit(
          pred = None,
          oss = Some(List(
            OrderSelection[Long](TimingWindowType / "id", true, true)
          )),
          offset = None,
          limit = None,
          child
        )
      }

    case (ObservationType, "attachments", Nil) =>
      Elab.transformChild { child =>
        OrderBy(OrderSelections(List(OrderSelection[Attachment.Id](AttachmentType / "id"))), child)
      }

    case (ObservationType, "itc", List(BooleanBinding.Option("useCache", rUseCache))) =>
      Elab.transformChild { child =>
          rUseCache.as(child)
      }

  }

  def itcQueryHandler: EffectHandler[F] = {
    // The Encoder[Itc] reassembles the pre-split GraphQL union JSON; see
    // lucuma.odb.json.itc.  A `Failed` acquisition is not representable there, so
    // it is surfaced as a field error below (as before the split).
    import lucuma.odb.json.itc.given
    import lucuma.odb.json.time.query.given
    import lucuma.odb.json.wavelength.query.given

    val readEnv: Env => Result[Unit] = _ => ().success

    val calculate: User ?=> (Program.Id, Observation.Id, Unit) => F[Result[Itc]] =
      (pid, oid, _) =>
        services.use { implicit s =>
          itcService
            .lookup(pid, oid)
            .map:
              case Left(e)  => e.asFailure
              case Right(i) =>
                // A deterministic acquisition failure is surfaced as a field
                // error, as before the split; the science part is unaffected
                // for sequence generation and workflow validation.
                i.acquisition match
                  case ItcAcquisition.Failed(msg) => OdbError.ItcError(msg.some).asFailure
                  case _                          => i.success
        }

    effectHandler(readEnv, calculate)
  }

  // Which requests apply to an observation is decided in Scala (`Configuration.subsumes`), but the
  // requests themselves are then fetched through SQL with the caller's selection, so that every
  // `ConfigurationRequest` field resolves exactly as it does on the program and top-level paths.
  // Serving them as JSON instead left any field the encoder didn't carry unselectable here.
  lazy val configurationRequestsQueryHandler: EffectHandler[F] = { pairs =>

    val keys: Result[List[(Program.Id, Observation.Id)]] =
      pairs.traverse: (_, cursor) =>
        (cursor.fieldAs[Program.Id]("programId"), cursor.fieldAs[Observation.Id]("id")).tupled

    // The applicable request ids for each pid+oid pair, in the order the service returns them.
    @annotation.nowarn("msg=unused implicit parameter")
    def applicable(ks: List[(Program.Id, Observation.Id)])(using Services[F], Transaction[F]): F[Result[Map[(Program.Id, Observation.Id), List[ConfigurationRequest.Id]]]] =
      configurationService.selectRequests(ks).map(_.map(_.view.mapValues(_.map(_.id)).toMap))

    // Fetches the requests `rids` through SQL with the selection `child`, returning the result list
    // cursor along with its elements keyed by id.
    def fetch(child: Query, env: Env, rids: List[ConfigurationRequest.Id]): F[Result[(Cursor, Map[ConfigurationRequest.Id, Cursor])]] =
      val q = Select("configurationRequests", None, Select("matches", None, Filter(Predicates.configurationRequest.id.in(rids), child)))
      sqlCursor(q, env).map: res =>
        for
          root    <- res
          matches <- root.field("configurationRequests", None).flatMap(_.field("matches", None))
          elems   <- matches.asList
          byId    <- elems.traverse(c => c.fieldAs[ConfigurationRequest.Id]("id").tupleRight(c))
        yield (matches, byId.toMap)

    // One SQL query per distinct child selection for the union of the ids its observations need,
    // split back out into one list per observation. Grackle runs that child selection against the
    // cursors we return, so aliases of this field that select the same fields share one query.
    def cursors(ridLists: List[List[ConfigurationRequest.Id]]): F[Result[List[Cursor]]] =
      pairs
        .zip(ridLists)
        .zipWithIndex
        .groupBy { case (((query, _), _), _) => Query.extractChild(query) }
        .toList
        .traverse: (child, group) =>
          val rids = group.flatMap(_._1._2).distinct
          val fetched: F[Result[Option[(Cursor, Map[ConfigurationRequest.Id, Cursor])]]] =
            child.toResultOrError("Configuration requests query has the wrong shape").flatTraverse: c =>
              if rids.isEmpty then Option.empty.success.pure[F]
              else fetch(c, group.head._1._1._2.fullEnv, rids).map(_.map(_.some))
          fetched.map: res =>
            res.flatMap: f =>
              group.traverse:
                case (((query, parent), rids), i) =>
                  val cursor: Result[Cursor] =
                    f match
                      case Some((matches, byId)) =>
                        val cs = rids.flatMap(byId.get)
                        ListTransformCursor(matches, cs.size, cs).success
                      case None                  =>
                        Query.childContext(parent.context, query).map: ctx =>
                          CirceCursor(ctx, Json.arr(), Some(parent), parent.fullEnv)
                  cursor.tupleRight(i)
        .map(_.sequence.map(_.flatten.sortBy(_._2).map(_._1)))

    UserEnv.traverse(UserEnv.fromQueries(pairs)):
      (for
        ks <- ResultT.fromResult(keys)
        m  <- ResultT(services.useTransactionally(applicable(ks)))
        cs <- ResultT(cursors(ks.map(k => m.getOrElse(k, Nil))))
      yield cs).value

  }

}
