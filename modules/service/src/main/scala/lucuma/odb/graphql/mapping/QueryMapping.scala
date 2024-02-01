// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.Resource
import cats.syntax.all._
import grackle.Path
import grackle.Predicate
import grackle.Predicate._
import grackle.Query
import grackle.Query._
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import lucuma.core.model
import lucuma.odb.data.ProgramReference
import lucuma.odb.data.Tag
import lucuma.odb.data.TargetRole
import lucuma.odb.graphql.binding._
import lucuma.odb.graphql.input.WhereDataset
import lucuma.odb.graphql.input.WhereExecutionEvent
import lucuma.odb.graphql.input.WhereObservation
import lucuma.odb.graphql.input.WhereProgram
import lucuma.odb.graphql.input.WhereTarget
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.predicate.ProgramPredicates
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
        SqlObject("datasets"),
        SqlObject("events"),
        SqlObject("filterTypeMeta"),
        SqlObject("obsAttachmentTypeMeta"),
        SqlObject("observation"),
        SqlObject("observations"),
        SqlObject("partnerMeta"),
        SqlObject("program"),
        SqlObject("programs"),
        SqlObject("proposalAttachmentTypeMeta"),
        SqlObject("proposalStatusMeta"),
        SqlObject("target"),
        SqlObject("targetGroup"),
        SqlObject("targets"),
      )
    )

  lazy val QueryElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    List(
      AsterismGroup,
      ConstraintSetGroup,
      Dataset,
      Datasets,
      Events,
      FilterTypeMeta,
      ObsAttachmentTypeMeta,
      Observation,
      Observations,
      PartnerMeta,
      Program,
      Programs,
      ProposalAttachmentTypeMeta,
      ProposalStatusMeta,
      Target,
      TargetGroup,
      Targets,
    ).combineAll

  // Elaborators below

  private lazy val AsterismGroup: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    val WhereObservationBinding = WhereObservation.binding(AsterismGroupType / "observations" / "matches")
    {
      case (QueryType, "asterismGroup", List(
        ProgramIdBinding.Option("programId", rPid),
        ProgramReferenceBinding.Option("programReference", rRef),
        WhereObservationBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      )) =>
        Elab.transformChild { child =>
          (programPredicate(rPid, rRef, Predicates.asterismGroup.program), rWHERE, rLIMIT, rIncludeDeleted).parTupled.flatMap { (program, WHERE, LIMIT, includeDeleted) =>
            val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
            ResultMapping.selectResult(child, limit) { q =>
              FilterOrderByOffsetLimit(
                pred = Some(
                  and(List(
                    WHERE.getOrElse(True),
                    program,
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
    }

  private lazy val ObsAttachmentTypeMeta: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "obsAttachmentTypeMeta", Nil) =>
      Elab.transformChild { child =>
        OrderBy(OrderSelections(List(OrderSelection[Tag](ObsAttachmentTypeMetaType / "tag"))), child)
      }

  private lazy val ProposalAttachmentTypeMeta: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "proposalAttachmentTypeMeta", Nil) =>
      Elab.transformChild { child =>
        OrderBy(OrderSelections(List(OrderSelection[Tag](ProposalAttachmentTypeMetaType / "tag"))), child)
      }

  private lazy val ProposalStatusMeta: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "proposalStatusMeta", Nil) =>
      Elab.transformChild { child =>
        OrderBy(OrderSelections(List(OrderSelection[Short](ProposalStatusMetaType / "ordinal"))), child)
      }

  private lazy val Dataset: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "dataset", List(
      DatasetIdBinding("datasetId", rDid)
    )) =>
      Elab.transformChild { child =>
        rDid.map { did =>
          Unique(
            Filter(
              And(
                Predicates.dataset.id.eql(did),
                Predicates.dataset.observation.program.isVisibleTo(user)
              ),
              child
            )
          )
        }
      }

  private lazy val Datasets: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    val WhereDatasetBinding = WhereDataset.binding(Path.from(DatasetType))
    {
      case (QueryType, "datasets", List(
        WhereDatasetBinding.Option("WHERE", rWHERE),
        DatasetIdBinding.Option("OFFSET", rOFFSET),
        NonNegIntBinding.Option("LIMIT", rLIMIT)
      )) =>
        Elab.transformChild { child =>
          (rWHERE, rOFFSET, rLIMIT).parTupled.flatMap { (WHERE, OFFSET, LIMIT) =>
            val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
            ResultMapping.selectResult(child, limit) { q =>
              FilterOrderByOffsetLimit(
                pred = Some(and(List(
                  OFFSET.map(Predicates.dataset.id.gtEql).getOrElse(True),
                  Predicates.dataset.observation.program.isVisibleTo(user),
                  WHERE.getOrElse(True)
                ))),
                oss = Some(List(
                  OrderSelection[lucuma.core.model.sequence.Dataset.Id](DatasetType / "id")
                )),
                offset = None,
                limit = Some(limit + 1), // Select one extra row here.
                child = q
              )
            }
          }
        }
    }
  

  private lazy val ConstraintSetGroup: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    val WhereObservationBinding = WhereObservation.binding(ConstraintSetGroupType / "observations" / "matches")
    {
      case (QueryType, "constraintSetGroup", List(
        ProgramIdBinding.Option("programId", rPid),
        ProgramReferenceBinding.Option("programReference", rRef),
        WhereObservationBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      )) =>
        Elab.transformChild { child =>
          (programPredicate(rPid, rRef, Predicates.constraintSetGroup.program), rWHERE, rLIMIT, rIncludeDeleted).parTupled.flatMap { (program, WHERE, LIMIT, includeDeleted) =>
            val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
            ResultMapping.selectResult(child, limit) { q =>
              FilterOrderByOffsetLimit(
                pred = Some(
                  and(List(
                    WHERE.getOrElse(True),
                    program,
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
      }

  private lazy val Events: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    val WhereExecutionEventBinding = WhereExecutionEvent.binding(Path.from(ExecutionEventType))
    {
      case (QueryType, "events", List(
        WhereExecutionEventBinding.Option("WHERE", rWHERE),
        ExecutionEventIdBinding.Option("OFFSET", rOFFSET),
        NonNegIntBinding.Option("LIMIT", rLIMIT)
      )) =>
        Elab.transformChild { child =>
          (rWHERE, rOFFSET, rLIMIT).parTupled.flatMap { (WHERE, OFFSET, LIMIT) =>
            val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
            ResultMapping.selectResult(child, limit) { q =>
              FilterOrderByOffsetLimit(
                pred = Some(and(List(
                  OFFSET.map(Predicates.executionEvent.id.gtEql).getOrElse(True),
                  Predicates.executionEvent.observation.program.isVisibleTo(user),
                  WHERE.getOrElse(True)
                ))),
                oss = Some(List(
                  OrderSelection[lucuma.core.model.ExecutionEvent.Id](ExecutionEventType / "id")
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

  private lazy val FilterTypeMeta: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "filterTypeMeta", Nil) =>
      Elab.transformChild { child =>
        OrderBy(OrderSelections(List(OrderSelection[Tag](FilterTypeMetaType / "tag"))), child)
      }

  private lazy val Observation: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "observation", List(
      ObservationIdBinding("observationId", rOid)
    )) =>
      Elab.transformChild { child =>
        rOid.map { oid =>
          Unique(
            Filter(
              And(
                Predicates.observation.id.eql(oid),
                Predicates.observation.program.isVisibleTo(user)
              ),
              child
            )
          )
        }
      }

  private lazy val Observations: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    val WhereObservationBinding = WhereObservation.binding(Path.from(ObservationType))
    {
      case (QueryType, "observations", List(
        WhereObservationBinding.Option("WHERE", rWHERE),
        ObservationIdBinding.Option("OFFSET", rOFFSET),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      )) =>
        Elab.transformChild { child =>
          (rWHERE, rOFFSET, rLIMIT, rIncludeDeleted).parTupled.flatMap { (WHERE, OFFSET, LIMIT, includeDeleted) =>
            val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
            ResultMapping.selectResult(child, limit) { q =>
              FilterOrderByOffsetLimit(
                pred = Some(and(List(
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
  }

  private lazy val PartnerMeta: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "partnerMeta", Nil) =>
      Elab.transformChild { child =>
        OrderBy(OrderSelections(List(OrderSelection[Tag](PartnerMetaType / "tag"))), child)
      }

  private def programPredicate(
    rPid: Result[Option[model.Program.Id]],
    rRef: Result[Option[ProgramReference]],
    prog: ProgramPredicates
  ): Result[Predicate] =
    (rPid, rRef).parTupled.map { (pid, ref) =>
      and(List(
        pid.map(prog.id.eql).toList,
        ref.map(r => prog.reference.eql(r.some)).toList
      ).flatten) match {
        case True => False // neither pid nor ref was supplied
        case p    => p
      }
    }

  private lazy val Program: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "program", List(
      ProgramIdBinding.Option("programId", rPid),
      ProgramReferenceBinding.Option("programReference", rRef)
    )) =>
      Elab.transformChild { child =>
        programPredicate(rPid, rRef, Predicates.program).map { program =>
          Unique(
            Filter(And(program, Predicates.program.isVisibleTo(user)), child)
          )
        }
      }

  private lazy val Programs: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    val WhereProgramBinding = WhereProgram.binding(Path.from(ProgramType))
    {
      case (QueryType, "programs", List(
        WhereProgramBinding.Option("WHERE", rWHERE),
        ProgramIdBinding.Option("OFFSET", rOFFSET),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      )) =>
        Elab.transformChild { child =>
          (rWHERE, rOFFSET, rLIMIT, rIncludeDeleted).parTupled.flatMap { (WHERE, OFFSET, LIMIT, includeDeleted) =>
            val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
            ResultMapping.selectResult(child, limit) { q =>
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
  }

  private lazy val Target: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "target", List(
      TargetIdBinding("targetId", rPid),
    )) =>
      Elab.transformChild { child =>
        rPid.map { pid =>
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
        }
      }

  private lazy val TargetGroup: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    val WhereObservationBinding = WhereObservation.binding(TargetGroupType / "observations" / "matches")
    {
      case (QueryType, "targetGroup", List(
        ProgramIdBinding.Option("programId", rPid),
        ProgramReferenceBinding.Option("programReference", rRef),
        WhereObservationBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      )) =>
        Elab.transformChild { child =>
          (programPredicate(rPid, rRef, Predicates.targetGroup.program), rWHERE, rLIMIT, rIncludeDeleted).parTupled.flatMap { (program, WHERE, LIMIT, includeDeleted) =>
            val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
            ResultMapping.selectResult(child, limit) { q =>
              FilterOrderByOffsetLimit(
                pred = Some(
                  and(List(
                    WHERE.getOrElse(True),
                    program,
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
    }
  }

  private lazy val Targets: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    val WhereTargetBinding = WhereTarget.binding(Path.from(TargetType))
    {
      case (QueryType, "targets", List(
        WhereTargetBinding.Option("WHERE", rWHERE),
        TargetIdBinding.Option("OFFSET", rOFFSET),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      )) =>
        Elab.transformChild { child =>
          (rWHERE, rOFFSET, rLIMIT, rIncludeDeleted).parTupled.flatMap { (WHERE, OFFSET, LIMIT, includeDeleted) =>
            val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
            ResultMapping.selectResult(child, limit) { q =>
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

}
