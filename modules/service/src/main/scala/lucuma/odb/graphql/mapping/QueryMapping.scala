// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.data.EitherT
import cats.data.NonEmptyList
import cats.effect.Resource
import cats.syntax.all._
import edu.gemini.grackle.Cursor
import edu.gemini.grackle.Cursor.Env
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import edu.gemini.grackle.syntax.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model
import lucuma.itc.client.ItcClient
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.input.WhereObservation
import lucuma.odb.graphql.input.WhereProgram
import lucuma.odb.graphql.input.WhereTargetInput
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.instances.given
import lucuma.odb.json.all.query.given
import lucuma.odb.logic.Generator
import lucuma.odb.logic.Itc
import lucuma.odb.logic.PlannedTimeCalculator
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*

import scala.reflect.ClassTag

trait QueryMapping[F[_]] extends Predicates[F] {
  this: SkunkMapping[F]
   with TargetMapping[F]
   with ObsAttachmentTypeMetaMapping[F]
   with FilterTypeMetaMapping[F]
   with PartnerMetaMapping[F]
   with ProgramMapping[F]
   with ProposalAttachmentTypeMetaMapping[F]
   with ObservationMapping[F] =>

  // Resources defined in the final cake.
  def commitHash: CommitHash
  def user: model.User
  def itcClient: ItcClient[F]
  def services: Resource[F, Services[F]]
  def plannedTimeCalculator: PlannedTimeCalculator.ForInstrumentMode

  // TODO: this is used by a now deprecated part of the schema.  Remove when possible.
  def itcQuery(
    path:     Path,
    pid:      model.Program.Id,
    oid:      model.Observation.Id,
    useCache: Boolean
  ): F[Result[Json]] =
    (for {
      p <- EitherT(services.useTransactionally(itc(itcClient).selectParams(pid, oid)))
      r <- EitherT(services.useNonTransactionally(itc(itcClient).callService(p, useCache)))
    } yield r).value.map {
      case Left(errors)     => Result.failure(errors.map(_.format).intercalate(", "))
      case Right(resultSet) => resultSet.asJson.success
    }

  // TODO: this is used by a now deprecated part of the schema.  Remove when possible.
  def sequence(
    path:        Path,
    pid:         model.Program.Id,
    oid:         model.Observation.Id,
    useCache:    Boolean,
    futureLimit: Generator.FutureLimit
  ): F[Result[Json]] = {

    val mapResult: Generator.Result => Result[Json] = {
      case Generator.Result.ObservationNotFound(_, _) => Result(Json.Null)
      case e: Generator.Error                         => Result.failure(e.format)
      case Generator.Result.Success(_, itc, exec, d)  =>
          Result(Json.obj(
            "programId"       -> pid.asJson,
            "observationId"   -> oid.asJson,
            "itcResult"       -> itc.asJson,
            "executionConfig" -> exec.asJson,
            "scienceDigest"   -> d.asJson
          ))
    }

    services.use { s =>
      val generatorService = s.generator(commitHash, itcClient, plannedTimeCalculator)

      (for {
        p <- EitherT(s.transactionally(generatorService.selectParams(pid, oid)))
        r <- EitherT(generatorService.generate(oid, p, useCache, futureLimit))
      } yield r).value.map(result => mapResult(result.merge))
    }

  }

  lazy val QueryMapping: ObjectMapping =
    ObjectMapping(
      tpe = QueryType,
      fieldMappings = List(
        SqlObject("asterismGroup"),
        SqlObject("constraintSetGroup"),
        SqlObject("filterTypeMeta"),
        RootEffect.computeJson("itc") { (_, path, env) =>
          val useCache = env.get[Boolean]("useCache").getOrElse(true)
          (env.getR[lucuma.core.model.Program.Id]("programId"),
           env.getR[lucuma.core.model.Observation.Id]("observationId")
          ).parTupled.flatTraverse { case (p, o) =>
            itcQuery(path, p, o, useCache)
          }
        },
        SqlObject("obsAttachmentTypeMeta"),
        SqlObject("observation"),
        SqlObject("observations"),
        SqlObject("partnerMeta"),
        SqlObject("program"),
        SqlObject("programs"),
        SqlObject("proposalAttachmentTypeMeta"),
        RootEffect.computeJson("sequence") { (_, path, env) =>
          val useCache    = env.get[Boolean]("useCache").getOrElse(true)
          val futureLimit = env.get[Generator.FutureLimit]("futureLimit").getOrElse(Generator.FutureLimit.Default)
          (env.getR[lucuma.core.model.Program.Id]("programId"),
           env.getR[lucuma.core.model.Observation.Id]("observationId")
          ).parTupled.flatTraverse { case (p, o) =>
            sequence(path, p, o, useCache, futureLimit)
          }
        },
        SqlObject("target"),
        SqlObject("targetGroup"),
        SqlObject("targets"),
      )
    )

  lazy val QueryElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    List(
      AsterismGroup,
      ConstraintSetGroup,
      FilterTypeMeta,
      Itc,
      ObsAttachmentTypeMeta,
      Observation,
      Observations,
      PartnerMeta,
      Program,
      Programs,
      ProposalAttachmentTypeMeta,
      Sequence,
      Target,
      TargetGroup,
      Targets,
    ).foldMap(pf => Map(QueryType -> pf))

  // Elaborators below

  private lazy val AsterismGroup: PartialFunction[Select, Result[Query]] =
    val WhereObservationBinding = WhereObservation.binding(AsterismGroupType / "observations" / "matches")
    {
      case Select("asterismGroup", List(
        ProgramIdBinding("programId", rProgramId),
        WhereObservationBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      ), child) =>
        (rProgramId, rWHERE, rLIMIT, rIncludeDeleted).parTupled.flatMap { (pid, WHERE, LIMIT, includeDeleted) =>
          val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
          ResultMapping.selectResult("asterismGroup", child, limit) { q =>
            FilterOrderByOffsetLimit(
              pred = Some(
                and(List(
                  WHERE.getOrElse(True),
                  Predicates.asterismGroup.program.id.eql(pid),
                  Predicates.asterismGroup.program.existence.includeDeleted(includeDeleted),
                  Predicates.asterismGroup.program.isVisibleTo(user),
                ))
              ),
              oss = None,
              offset = None,
              limit = Some(limit + 1), // Select one extra row here.
              child = q
            )
          }
        }
    }

  private lazy val ObsAttachmentTypeMeta: PartialFunction[Select, Result[Query]] =
    case Select("obsAttachmentTypeMeta", Nil, child) =>
      Result(Select("obsAttachmentTypeMeta", Nil,
        OrderBy(OrderSelections(List(OrderSelection[Tag](ObsAttachmentTypeMetaType / "tag"))), child)
      ))

  private lazy val ProposalAttachmentTypeMeta: PartialFunction[Select, Result[Query]] =
    case Select("proposalAttachmentTypeMeta", Nil, child) =>
      Result(Select("proposalAttachmentTypeMeta", Nil,
        OrderBy(OrderSelections(List(OrderSelection[Tag](ProposalAttachmentTypeMetaType / "tag"))), child)
      ))

  private lazy val ConstraintSetGroup: PartialFunction[Select, Result[Query]] =
    val WhereObservationBinding = WhereObservation.binding(ConstraintSetGroupType / "observations" / "matches")
    {
      case Select("constraintSetGroup", List(
        ProgramIdBinding("programId", rProgramId),
        WhereObservationBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      ), child) =>
        (rProgramId, rWHERE, rLIMIT, rIncludeDeleted).parTupled.flatMap { (pid, WHERE, LIMIT, includeDeleted) =>
          val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
          ResultMapping.selectResult("constraintSetGroup", child, limit) { q =>
            FilterOrderByOffsetLimit(
              pred = Some(
                and(List(
                  WHERE.getOrElse(True),
                  Predicates.constraintSetGroup.programId.eql(pid),
                  Predicates.constraintSetGroup.observations.matches.existence.includeDeleted(includeDeleted),
                  Predicates.constraintSetGroup.observations.matches.program.existence.includeDeleted(includeDeleted),
                  Predicates.constraintSetGroup.observations.matches.program.isVisibleTo(user),
                ))
              ),
              oss = Some(List(
                OrderSelection[String](ConstraintSetGroupType / "key")
              )),
              offset = None,
              limit = Some(limit + 1), // Select one extra row here.
              child = q
            )
          }
        }
    }

  private lazy val FilterTypeMeta: PartialFunction[Select, Result[Query]] =
    case Select("filterTypeMeta", Nil, child) =>
      Result(Select("filterTypeMeta", Nil,
        OrderBy(OrderSelections(List(OrderSelection[Tag](FilterTypeMetaType / "tag"))), child)
      ))

  private lazy val Itc: PartialFunction[Select, Result[Query]] =
    case Select("itc", List(
      ProgramIdBinding("programId", rPid),
      ObservationIdBinding("observationId", rOid),
      BooleanBinding("useCache", rUseCache)
    ), child) =>
      (rPid, rOid, rUseCache).parTupled.map { case (pid, oid, useCache) =>
        Environment(
          Env("programId" -> pid, "observationId" -> oid, "useCache" -> useCache),
          Select("itc", Nil, child)
        )
      }

  private lazy val Observation: PartialFunction[Select, Result[Query]] =
    case Select("observation", List(
      ObservationIdBinding("observationId", rOid)
    ), child) =>
      rOid.map { oid =>
        Select("observation", Nil,
          Unique(
            Filter(
              And(
                Predicates.observation.id.eql(oid),
                Predicates.observation.program.isVisibleTo(user)
              ),
              child
            )
          )
        )
      }

  private lazy val Observations: PartialFunction[Select, Result[Query]] = {
    val WhereObservationBinding = WhereObservation.binding(Path.from(ObservationType))
    {
      case Select("observations", List(
        ProgramIdBinding.Option("programId", rPid),
        WhereObservationBinding.Option("WHERE", rWHERE),
        ObservationIdBinding.Option("OFFSET", rOFFSET),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      ), child) =>
        (rPid, rWHERE, rOFFSET, rLIMIT, rIncludeDeleted).parTupled.flatMap { (pid, WHERE, OFFSET, LIMIT, includeDeleted) =>
          val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
          ResultMapping.selectResult("observations", child, limit) { q =>
            FilterOrderByOffsetLimit(
              pred = Some(and(List(
                pid.map(Predicates.observation.program.id.eql).getOrElse(True),
                OFFSET.map(Predicates.observation.id.gtEql).getOrElse(True),
                Predicates.observation.existence.includeDeleted(includeDeleted),
                Predicates.observation.program.isVisibleTo(user),
                WHERE.getOrElse(True)
              ))),
              oss = Some(List(
                OrderSelection[lucuma.core.model.Observation.Id](ObservationType / "id")
              )),
              offset = None,
              limit = Some(limit + 1), // Select one extra row here.
              child = q
            )
          }
        }
      }
    }

  private lazy val PartnerMeta: PartialFunction[Select, Result[Query]] =
    case Select("partnerMeta", Nil, child) =>
      Result(Select("partnerMeta", Nil,
        OrderBy(OrderSelections(List(OrderSelection[Tag](PartnerMetaType / "tag"))), child)
      ))

  private lazy val Program: PartialFunction[Select, Result[Query]] =
    case Select("program", List(
      ProgramIdBinding("programId", rPid),
    ), child) =>
      rPid.map { pid =>
        Select("program", Nil,
          Unique(
            Filter(
              And(
                Predicates.program.id.eql(pid),
                Predicates.program.isVisibleTo(user),
              ),
              child
            )
          )
        )
      }

  private lazy val Programs: PartialFunction[Select, Result[Query]] = {
    val WhereProgramBinding = WhereProgram.binding(Path.from(ProgramType))
    {
      case Select("programs", List(
        WhereProgramBinding.Option("WHERE", rWHERE),
        ProgramIdBinding.Option("OFFSET", rOFFSET),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      ), child) =>
        (rWHERE, rOFFSET, rLIMIT, rIncludeDeleted).parTupled.flatMap { (WHERE, OFFSET, LIMIT, includeDeleted) =>
          val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
          ResultMapping.selectResult("programs", child, limit) { q =>
            FilterOrderByOffsetLimit(
              pred = Some(
                and(List(
                  OFFSET.map(Predicates.program.id.gtEql).getOrElse(True),
                  Predicates.program.existence.includeDeleted(includeDeleted),
                  Predicates.program.isVisibleTo(user),
                  WHERE.getOrElse(True)
                ))
              ),
              oss = Some(List(
                OrderSelection[lucuma.core.model.Program.Id](ProgramType / "id")
              )),
              offset = None,
              limit = Some(limit + 1), // Select one extra row here.
              child = q
            )
          }
        }
      }
  }

  private lazy val Sequence: PartialFunction[Select, Result[Query]] =
    case Select("sequence", List(
      ProgramIdBinding("programId", rPid),
      ObservationIdBinding("observationId", rOid),
      BooleanBinding("useCache", rUseCache),
      Generator.FutureLimit.Binding("futureLimit", rFutureLimit)
    ), child) =>
      (rPid, rOid, rUseCache, rFutureLimit).parTupled.map { case (pid, oid, useCache, futureLimit) =>
        Environment(
          Env(
            "programId"     -> pid,
            "observationId" -> oid,
            "useCache"      -> useCache,
            "futureLimit"   -> futureLimit
          ),
          Select("sequence", Nil, child)
        )
      }

  private lazy val Target: PartialFunction[Select, Result[Query]] =
    case Select("target", List(
      TargetIdBinding("targetId", rPid),
    ), child) =>
      rPid.map { pid =>
        Select("target", Nil,
          Unique(
            Filter(
              And(
                Predicates.target.id.eql(pid),
                Predicates.target.program.isVisibleTo(user),
              ),
              child
            )
          )
        )
      }

  private lazy val TargetGroup: PartialFunction[Select, Result[Query]] =
    val WhereObservationBinding = WhereObservation.binding(TargetGroupType / "observations" / "matches")
    {
      case Select("targetGroup", List(
        ProgramIdBinding("programId", rProgramId),
        WhereObservationBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      ), child) =>
        (rProgramId, rWHERE, rLIMIT, rIncludeDeleted).parTupled.flatMap { (pid, WHERE, LIMIT, includeDeleted) =>
          val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
          ResultMapping.selectResult("targetGroup", child, limit) { q =>
            FilterOrderByOffsetLimit(
              pred = Some(
                and(List(
                  WHERE.getOrElse(True),
                  Predicates.targetGroup.program.id.eql(pid),
                  Predicates.targetGroup.target.existence.includeDeleted(includeDeleted),
                  Predicates.targetGroup.program.isVisibleTo(user),
                ))
              ),
              oss = Some(List(
                OrderSelection[lucuma.core.model.Target.Id](TargetGroupType / "key")
              )),
              offset = None,
              limit = Some(limit + 1), // Select one extra row here.
              child = q
            )
          }
        }
    }

  private lazy val Targets: PartialFunction[Select, Result[Query]] = {
    val WhereTargetInputBinding = WhereTargetInput.binding(Path.from(TargetType))
    {
      case Select("targets", List(
        WhereTargetInputBinding.Option("WHERE", rWHERE),
        TargetIdBinding.Option("OFFSET", rOFFSET),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      ), child) =>
        (rWHERE, rOFFSET, rLIMIT, rIncludeDeleted).parTupled.flatMap { (WHERE, OFFSET, LIMIT, includeDeleted) =>
          val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
          ResultMapping.selectResult("targets", child, limit) { q =>
            FilterOrderByOffsetLimit(
              pred = Some(
                and(List(
                  OFFSET.map(Predicates.target.id.gtEql).getOrElse(True),
                  Predicates.target.existence.includeDeleted(includeDeleted),
                  Predicates.target.program.isVisibleTo(user),
                  WHERE.getOrElse(True)
                )
              )),
              oss = Some(List(
                OrderSelection[lucuma.core.model.Target.Id](TargetType / "id")
              )),
              offset = None,
              limit = Some(limit + 1), // Select one extra row here.
              child = q
            )
          }
        }
      }
  }

}
