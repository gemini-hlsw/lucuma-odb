// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.Resource
import cats.syntax.all.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import grackle.Query
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import lucuma.core.model
import lucuma.core.model.CallForProposals
import lucuma.core.model.ObservationReference
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.core.model.sequence.DatasetReference
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.WhereCallForProposals
import lucuma.odb.graphql.input.WhereDataset
import lucuma.odb.graphql.input.WhereExecutionEvent
import lucuma.odb.graphql.input.WhereObservation
import lucuma.odb.graphql.input.WhereProgram
import lucuma.odb.graphql.input.WhereProgramUser
import lucuma.odb.graphql.input.WhereSpectroscopyConfigOption
import lucuma.odb.graphql.input.WhereTarget
import lucuma.odb.graphql.predicate.DatasetPredicates
import lucuma.odb.graphql.predicate.ObservationPredicates
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.predicate.ProgramPredicates
import lucuma.odb.instances.given
import lucuma.odb.service.Services
import lucuma.core.model.ConfigurationRequest
import lucuma.odb.graphql.input.WhereConfigurationRequest

trait QueryMapping[F[_]] extends Predicates[F] {
  this: SkunkMapping[F] =>

  // Resources defined in the final cake.
  def user: model.User
  def services: Resource[F, Services[F]]

  lazy val QueryMapping: ObjectMapping =
    ObjectMapping(QueryType)(
      SqlObject("asterismGroup"),
      SqlObject("callForProposals"),
      SqlObject("callsForProposals"),
      SqlObject("configurationRequests"),
      SqlObject("constraintSetGroup"),
      SqlObject("dataset"),
      SqlObject("datasets"),
      SqlObject("events"),
      SqlObject("filterTypeMeta"),
      SqlObject("group"),
      SqlObject("observation"),
      SqlObject("observations"),
      SqlObject("observingModeGroup"),
      SqlObject("program"),
      SqlObject("programs"),
      SqlObject("programUsers"),
      SqlObject("proposalStatusMeta"),
      SqlObject("spectroscopyConfigOptions"),
      SqlObject("target"),
      SqlObject("targetGroup"),
      SqlObject("targets"),
    )

  lazy val QueryElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    List(
      AsterismGroup,
      CallForProposals,
      CallsForProposals,
      ConfigurationRequests,
      ConstraintSetGroup,
      Dataset,
      Datasets,
      Events,
      FilterTypeMeta,
      Group,
      Observation,
      Observations,
      ObservingModeGroup,
      Program,
      Programs,
      ProgramUsers,
      ProposalStatusMeta,
      SpectroscopyConfigOptions,
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
        ProposalReferenceBinding.Option("proposalReference", rProp),
        ProgramReferenceBinding.Option("programReference", rProg),
        WhereObservationBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      )) =>
        Elab.transformChild { child =>
          (programPredicate(rPid, rProp, rProg, Predicates.asterismGroup.program), rWHERE, rLIMIT, rIncludeDeleted).parTupled.flatMap { (program, WHERE, LIMIT, includeDeleted) =>
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

  private lazy val CallForProposals: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    {
      case (QueryType, "callForProposals", List(
        CallForProposalsIdBinding("callForProposalsId", rCid)
      )) =>
        Elab.transformChild { child =>
          rCid.map { cid =>
            Unique(
              Filter(
                Predicates.callForProposals.id.eql(cid),
                child
              )
            )
          }
        }
    }

  private lazy val CallsForProposals: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    val WhereCallForProposalsBinding = WhereCallForProposals.binding(Path.from(CallForProposalsType))
    {
      case (QueryType, "callsForProposals", List(
        WhereCallForProposalsBinding.Option("WHERE", rWHERE),
        CallForProposalsIdBinding.Option("OFFSET", rOFFSET),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      )) =>
        Elab.transformChild { child =>
          (rWHERE, rOFFSET, rLIMIT, rIncludeDeleted).parTupled.flatMap { (WHERE, OFFSET, LIMIT, includeDeleted) =>
            val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
            ResultMapping.selectResult(child, limit) { q =>
              FilterOrderByOffsetLimit(
                pred = Some(and(List(
                  OFFSET.map(Predicates.callForProposals.id.gtEql).getOrElse(True),
                  Predicates.callForProposals.existence.includeDeleted(includeDeleted),
                  WHERE.getOrElse(True)
                ))),
                oss = Some(List(
                  OrderSelection[lucuma.core.model.CallForProposals.Id](CallForProposalsType / "id")
                )),
                offset = None,
                limit = Some(limit + 1), // Select one extra row here.
                child = q
              )
            }
          }
        }
    }

  private lazy val ProposalStatusMeta: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "proposalStatusMeta", Nil) =>
      Elab.transformChild { child =>
        OrderBy(OrderSelections(List(OrderSelection[Short](ProposalStatusMetaType / "ordinal"))), child)
      }

  private def datasetPredicate(
    rDid: Result[Option[model.sequence.Dataset.Id]],
    rRef: Result[Option[DatasetReference]],
    pred: DatasetPredicates
  ): Result[Predicate] =
    (rDid, rRef).parTupled.map { (did, ref) =>
      and(List(
        did.map(pred.id.eql).toList,
        ref.map(r => pred.referenceLabel.eql(r)).toList
      ).flatten) match {
        case True => False // neither did nor ref was supplied
        case p    => p
      }
    }

  private lazy val Dataset: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "dataset", List(
      DatasetIdBinding.Option("datasetId", rDid),
      DatasetReferenceBinding.Option("datasetReference", rRef)
    )) =>
      Elab.transformChild { child =>
        datasetPredicate(rDid, rRef, Predicates.dataset).map { dataset =>
          Unique(
            Filter(
              And(dataset, Predicates.dataset.observation.program.isVisibleTo(user)),
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
        ProposalReferenceBinding.Option("proposalReference", rProp),
        ProgramReferenceBinding.Option("programReference", rProg),
        WhereObservationBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      )) =>
        Elab.transformChild { child =>
          (programPredicate(rPid, rProp, rProg, Predicates.constraintSetGroup.program), rWHERE, rLIMIT, rIncludeDeleted).parTupled.flatMap { (program, WHERE, LIMIT, includeDeleted) =>
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

  private lazy val Group: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "group", List(
      GroupIdBinding("groupId", rGroup)
    )) =>
      Elab.transformChild { child =>
        rGroup.map { grp =>
          Unique(
            Filter(
              And(
                Predicates.group.id.eql(grp),
                Predicates.group.program.isVisibleTo(user)
              ),
              child
            )
          )
        }
      }

  private def observationPredicate(
    rOid: Result[Option[model.Observation.Id]],
    rRef: Result[Option[ObservationReference]],
    pred: ObservationPredicates
  ): Result[Predicate] =
    (rOid, rRef).parTupled.map { (oid, ref) =>
      and(List(
        oid.map(pred.id.eql).toList,
        ref.map(r => pred.referenceLabel.eql(r)).toList
      ).flatten) match {
        case True => False // neither oid nor ref was supplied
        case p    => p
      }
    }

  private lazy val Observation: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "observation", List(
      ObservationIdBinding.Option("observationId", rOid),
      ObservationReferenceBinding.Option("observationReference", rRef)
    )) =>
      Elab.transformChild { child =>
        observationPredicate(rOid, rRef, Predicates.observation).map { obs =>
          Unique(
            Filter(And(obs, Predicates.observation.program.isVisibleTo(user)), child)
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

  private lazy val ConfigurationRequests: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    val WhereConfigurationRequestsBinding = WhereConfigurationRequest.binding(Path.from(ConfigurationRequestType))
    {
      case (QueryType, "configurationRequests", List(
        WhereConfigurationRequestsBinding.Option("WHERE", rWHERE),
        ConfigurationRequestIdBinding.Option("OFFSET", rOFFSET),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
      )) =>
        Elab.transformChild { child =>
          (rWHERE, rOFFSET, rLIMIT).parTupled.flatMap { (WHERE, OFFSET, lim) =>
            val limit = lim.fold(ResultMapping.MaxLimit)(_.value)
            ResultMapping.selectResult(child, limit) { q =>
              FilterOrderByOffsetLimit(
                pred = Some(and(List(
                  OFFSET.fold[Predicate](True)(Predicates.configurationRequest.id.gtEql),
                  WHERE.getOrElse(True),
                  Predicates.configurationRequest.program.isVisibleTo(user),
                ))),
                oss = Some(List(OrderSelection[ConfigurationRequest.Id](ConfigurationRequestType / "id"))),
                offset = None,
                limit = Some(limit + 1),
                q
              )
            }
          }
        }
    }

  private lazy val ObservingModeGroup: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    val WhereObservationBinding = WhereObservation.binding(ObservingModeGroupType / "observations" / "matches")
    {
      case (QueryType, "observingModeGroup", List(
        ProgramIdBinding.Option("programId", rPid),
        ProposalReferenceBinding.Option("proposalReference", rProp),
        ProgramReferenceBinding.Option("programReference", rProg),
        WhereObservationBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      )) =>
        Elab.transformChild: child =>
          (programPredicate(rPid, rProp, rProg, Predicates.observingModeGroup.program), rWHERE, rLIMIT, rIncludeDeleted).parTupled.flatMap: (program, WHERE, LIMIT, includeDeleted) =>
            val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
            ResultMapping.selectResult(child, limit): q =>
              FilterOrderByOffsetLimit(
                pred = and(List(
                  WHERE.getOrElse(True),
                  program,
                  Predicates.observingModeGroup.observations.matches.existence.includeDeleted(includeDeleted),
                  Predicates.observingModeGroup.observations.matches.program.existence.includeDeleted(includeDeleted),
                  Predicates.observingModeGroup.observations.matches.program.isVisibleTo(user),
                )).some,
                oss    = List(OrderSelection[String](ObservingModeGroupType / "key")).some,
                offset = none,
                limit  = (limit + 1).some, // Select one extra row here.
                child  = q
              )
    }

  private def programPredicate(
    rPid:  Result[Option[model.Program.Id]],
    rProp: Result[Option[ProposalReference]],
    rProg: Result[Option[ProgramReference]],
    prog:  ProgramPredicates
  ): Result[Predicate] =
    (rPid, rProp, rProg).parTupled.map { (pid, op, og) =>
      and(List(
        pid.map(prog.id.eql).toList,
        op.map(r => prog.proposal.referenceLabel.eql(r)).toList,
        og.map(r => prog.referenceLabel.eql(r)).toList
      ).flatten) match {
        case True => False // neither pid nor ref nor pro was supplied
        case p    => p
      }
    }

  private lazy val Program: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "program", List(
      ProgramIdBinding.Option("programId", rPid),
      ProposalReferenceBinding.Option("proposalReference", rProp),
      ProgramReferenceBinding.Option("programReference", rProg)
    )) =>
      Elab.transformChild { child =>
        programPredicate(rPid, rProp, rProg, Predicates.program).map { program =>
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

  private lazy val ProgramUsers: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    val WhereProgramUserBinding = WhereProgramUser.binding(Path.from(ProgramUserType))
    {
      case (QueryType, "programUsers", List(
        WhereProgramUserBinding.Option("WHERE", rWHERE),
        UserIdBinding.Option("OFFSET", rOFFSET),
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
                    OFFSET.map(Predicates.programUser.userId.gtEql).getOrElse(True),
                    Predicates.programUser.program.existence.includeDeleted(includeDeleted),
                    Predicates.programUser.program.isVisibleTo(user),
                    WHERE.getOrElse(True)
                  ))
                ),
                oss = Some(List(
                  OrderSelection[lucuma.core.model.User.Id](ProgramUserType / "userId"),
                  OrderSelection[lucuma.core.model.Program.Id](ProgramUserType / "programId")
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

  private lazy val SpectroscopyConfigOptions: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    val WhereOptions = WhereSpectroscopyConfigOption.binding(Path.from(SpectroscopyConfigOptionType))
    {
      case (QueryType, "spectroscopyConfigOptions", List(
        WhereOptions.Option("WHERE", rWHERE)
      )) =>
        Elab.transformChild { child =>
          rWHERE.map { where =>
            Filter(where.getOrElse(True), child)
          }
        }
    }
  }

  private lazy val Target: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "target", List(
      TargetIdBinding("targetId", rTid),
    )) =>
      Elab.transformChild { child =>
        rTid.map { tid =>
          Unique(
            Filter(
              and(List(
                Predicates.target.id.eql(tid),
                Predicates.target.program.isVisibleTo(user),
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
        ProposalReferenceBinding.Option("proposalReference", rProp),
        ProgramReferenceBinding.Option("programReference", rProg),
        WhereObservationBinding.Option("WHERE", rWHERE),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      )) =>
        Elab.transformChild { child =>
          (programPredicate(rPid, rProp, rProg, Predicates.targetGroup.program), rWHERE, rLIMIT, rIncludeDeleted).parTupled.flatMap { (program, WHERE, LIMIT, includeDeleted) =>
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
