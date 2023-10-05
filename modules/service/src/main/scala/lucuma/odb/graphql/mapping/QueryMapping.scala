// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.Resource
import cats.syntax.all._
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate._
import edu.gemini.grackle.Query
import edu.gemini.grackle.Query._
import edu.gemini.grackle.Result
import edu.gemini.grackle.TypeRef
import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.core.model
import lucuma.odb.data.Tag
import lucuma.odb.data.TargetRole
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.input.DatasetIdInput
import lucuma.odb.graphql.input.WhereObservation
import lucuma.odb.graphql.input.WhereProgram
import lucuma.odb.graphql.input.WhereTargetInput
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.instances.given
import lucuma.odb.service.Services

trait QueryMapping[F[_]] extends Predicates[F] {
  this: SkunkMapping[F] =>

  // Resources defined in the final cake.
  def user: model.User
  def services: Resource[F, Services[F]]

  lazy val QueryMapping: ObjectMapping =
    ObjectMapping(
      tpe = QueryType,
      fieldMappings = List(
        SqlObject("asterismGroup"),
        SqlObject("constraintSetGroup"),
        SqlObject("dataset"),
        SqlObject("filterTypeMeta"),
        SqlObject("obsAttachmentTypeMeta"),
        SqlObject("observation"),
        SqlObject("observations"),
        SqlObject("partnerMeta"),
        SqlObject("program"),
        SqlObject("programs"),
        SqlObject("proposalAttachmentTypeMeta"),
        SqlObject("target"),
        SqlObject("targetGroup"),
        SqlObject("targets"),
      )
    )

  lazy val QueryElaborator: Map[TypeRef, PartialFunction[Select, Result[Query]]] =
    List(
      AsterismGroup,
      ConstraintSetGroup,
      Dataset,
      FilterTypeMeta,
      ObsAttachmentTypeMeta,
      Observation,
      Observations,
      PartnerMeta,
      Program,
      Programs,
      ProposalAttachmentTypeMeta,
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

  private lazy val Dataset: PartialFunction[Select, Result[Query]] =
    case Select("dataset", List(
      DatasetIdInput.Binding("datasetId", rDid)
    ), child) =>
      rDid.map { did =>
        Select("dataset", Nil,
          Unique(
            Filter(
//              And(
                Predicates.dataset.id.eql(did),
//                Predicates.dataset.observation.program.isVisibleTo(user)
//              ),
              child
            )
          )
        )
      }

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

  private lazy val Target: PartialFunction[Select, Result[Query]] =
    case Select("target", List(
      TargetIdBinding("targetId", rPid),
    ), child) =>
      rPid.map { pid =>
        Select("target", Nil,
          Unique(
            Filter(
              and(List(
                Predicates.target.id.eql(pid),
                Predicates.target.program.isVisibleTo(user),
                Predicates.target.hasRole(TargetRole.Science)
              )),
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
                  Predicates.target.hasRole(TargetRole.Science),
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
