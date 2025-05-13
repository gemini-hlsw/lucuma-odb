// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.Order.catsKernelOrderingForOrder
import cats.effect.Resource
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Context
import grackle.Env
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import grackle.Query
import grackle.Query.*
import grackle.QueryCompiler.Elab
import grackle.QueryInterpreter
import grackle.Result
import grackle.ResultT
import grackle.TypeRef
import grackle.skunk.SkunkMapping
import grackle.syntax.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model
import lucuma.core.model.Access
import lucuma.core.model.CallForProposals
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ObservationReference
import lucuma.core.model.OrcidId
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.model.sequence.DatasetReference
import lucuma.itc.client.ItcClient
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.WhereCallForProposals
import lucuma.odb.graphql.input.WhereConfigurationRequest
import lucuma.odb.graphql.input.WhereDataset
import lucuma.odb.graphql.input.WhereExecutionEvent
import lucuma.odb.graphql.input.WhereImagingConfigOption
import lucuma.odb.graphql.input.WhereObservation
import lucuma.odb.graphql.input.WhereProgram
import lucuma.odb.graphql.input.WhereProgramNote
import lucuma.odb.graphql.input.WhereProgramUser
import lucuma.odb.graphql.input.WhereSpectroscopyConfigOption
import lucuma.odb.graphql.input.WhereTarget
import lucuma.odb.graphql.predicate.DatasetPredicates
import lucuma.odb.graphql.predicate.ObservationPredicates
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.predicate.ProgramPredicates
import lucuma.odb.instances.given
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services
import skunk.Transaction

trait QueryMapping[F[_]] extends Predicates[F] {
  this: SkunkMapping[F] =>

  // Resources defined in the final cake.
  def user: model.User
  def services: Resource[F, Services[F]]
  def timeEstimateCalculator: TimeEstimateCalculatorImplementation.ForInstrumentMode
  def itcClient: ItcClient[F]
  def commitHash: CommitHash
  def goaUsers: Set[User.Id]

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
      RootEffect.computeJson("goaDataDownloadAccess")(goaDataDownloadAccess),
      SqlObject("group"),
      SqlObject("observation"),
      SqlObject("observations"),
      RootEffect.computeChild("observationsByWorkflowState")(observationsByWorkflowState),
      SqlObject("observingModeGroup"),
      SqlObject("program"),
      SqlObject("programs"),
      SqlObject("programNote"),
      SqlObject("programNotes"),
      SqlObject("programUsers"),
      SqlObject("proposalStatusMeta"),
      SqlObject("spectroscopyConfigOptions"),
      SqlObject("imagingConfigOptions"),
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
      GoaDataDownloadAccess,
      Group,
      ImagingConfigOptions,
      Observation,
      Observations,
      ObservationsByWorkflowState,
      ObservingModeGroup,
      Program,
      Programs,
      ProgramNote,
      ProgramNotes,
      ProgramUsers,
      ProposalStatusMeta,
      SpectroscopyConfigOptions,
      Target,
      TargetGroup,
      Targets,
    ).combineAll

  import Services.Syntax.*

  // We can't directly filter for computed values like workflow state, so our strategy here
  // is to first fetch the oids of the observations that match the WhereObservation, then
  // fetch the workflow states for just those oids, then find the ones with the states we're
  // interested in and build a new query that's the same as the original but with a swapped
  // out predicate that explicitly matches the set of oids we have computed.
  def observationsByWorkflowState(query: Query, p: Path, e: Env): F[Result[Query]] =
    query match {
      case Environment(env, Filter(pred, child)) =>

        // observation query result to list of oids
        def decode(json: Json): Result[List[model.Observation.Id]] =
          Result.fromEither:
            json
              .hcursor.downFields("observations", "matches").as[List[Json]].flatMap: jsons =>
                jsons.traverse(_.hcursor.downField("id").as[model.Observation.Id])
              .leftMap(_.toString)

        // given an observation predicate, find ids of matching observations
        def oids(pred: Predicate): ResultT[F, List[model.Observation.Id]] =
          val idQuery    = Select("observations", Select("matches", Filter(pred, Select("id"))))
          val rootCursor = RootCursor(Context(QueryType), None, env)
          for
            pjson <- ResultT(interpreter.runOneShot(idQuery, QueryType, rootCursor))
            json  <- ResultT(QueryInterpreter.complete[F](pjson))
            oids  <- ResultT.fromResult(decode(json))
          yield oids

        // given a list of oids, find their workflows
        def stateMap(oids: List[model.Observation.Id])(using Services[F], NoTransaction[F]) : F[Result[Map[model.Observation.Id, ObservationWorkflowState]]] =
          Services.asSuperUser:
            observationWorkflowService.getWorkflows(oids, commitHash, itcClient, timeEstimateCalculator).map: r =>
              r.map: m =>
                m.view.mapValues(_.state).toMap

        // a new query with a predicate over the specified oids, with ordering by oid
        def newQuery(oids: List[model.Observation.Id], child: Query): Query =
          FilterOrderByOffsetLimit(
            pred = Some(In(ObservationType / "id", oids)),
            oss = Some(List(
              OrderSelection[model.Observation.Id](ObservationType / "id")
            )),
            offset = None,
            limit = None,
            child
          )

        // Ok here we go. Figure out which observations we're talking about, filter based on state, and return a
        // query that will just fetch the matches.
        def go(using Services[F], NoTransaction[F]): ResultT[F, Query] =
          for
            states   <- ResultT.fromResult(env.getR[List[ObservationWorkflowState]]("states"))
            oids     <- oids(pred)
            stateMap <- ResultT(stateMap(oids))
            filtered  = oids.filter(stateMap.get(_).exists(states.contains))
          yield Environment(env, newQuery(filtered, child))

        // I can't believe this works.
        services.useNonTransactionally:
          requireStaffAccess: // N.B. this is only for staff or better, at least for now
            go.value

      case other =>
        Result.internalError(s"observationsByWorkflowState: expected Environment(env, Filter(pred, child)) got $other").pure[F]

    }

  private val goaDataDownloadAccess: (Path, Env) => F[Result[Json]] = (p, e) =>
    val notAuthorized = OdbError.NotAuthorized(user.id, "Only the GOA user may access this field.".some).asFailure
    val goaUserCheck  = user match
      case StandardUser(id, r, rs, _) => notAuthorized.unlessA(goaUsers.contains(id) || ((r::rs).map(_.access).max >= Access.Staff))
      case _                          => notAuthorized

    def decode(r: Json): Result[List[ProgramReference]] =
      r.hcursor.downFields("programUsers", "matches").values.toList.flatMap(_.toList).traverse: json =>
        json
          .hcursor
          .downFields("program", "reference", "label")
          .as[String]
          .toOption
          .flatMap(ProgramReference.fromString.getOption)
          .fold(Result.internalError[ProgramReference](s"Could not parse program reference label results: ${json.spaces2}"))(_.success)

    def refs(orcidId: OrcidId): ResultT[F, List[ProgramReference]] =
      val pred     = and(List(
        Predicates.programUser.program.referenceLabel.isNull(false),
        Eql(Path.from(ProgramUserType) / "user" / "orcidId", Const(orcidId.value.some)),
        Predicates.programUser.hasDataAccess.eql(true)
      ))
      val refQuery   = Select("programUsers", Select("matches", Filter(pred, Select("program", Select("reference", Select("label"))))))
      val rootCursor = RootCursor(Context(QueryType), None, Env.EmptyEnv)
      for
        pjson <- ResultT(interpreter.runOneShot(refQuery, QueryType, rootCursor))
        json  <- ResultT(QueryInterpreter.complete[F](pjson))
        mids  <- ResultT.fromResult(decode(json))
      yield mids

    def go(using Services[F], Transaction[F]): ResultT[F, List[ProgramReference]] =
      for
        _     <- ResultT.fromResult(goaUserCheck)
        orcid <- ResultT.fromResult(e.getR[OrcidId]("orcidId"))
        refs  <- refs(orcid)
      yield refs.distinct.sorted

    services.useTransactionally:
      go.map(_.map(_.asJson).asJson).value

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

  private lazy val GoaDataDownloadAccess: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    case (QueryType, "goaDataDownloadAccess", List(
      OrcidIdBinding("orcidId", rOrcidId)
    )) =>
      Elab.liftR(rOrcidId).flatMap(orcidId => Elab.env("orcidId", orcidId))

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

  private lazy val ObservationsByWorkflowState: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    val WhereObservationBinding = WhereObservation.binding(Path.from(ObservationType))
    val StateBinding = enumeratedBinding[ObservationWorkflowState]
    {
      case (QueryType, "observationsByWorkflowState", List(
        WhereObservationBinding.Option("WHERE", rWHERE),
        StateBinding.List.Option("states", rStates)
      )) =>
        Elab.transformChild { child =>
          (rWHERE, rStates).parMapN { (WHERE, states) =>
            Environment(
              Env("states" -> states.orEmpty),
              Filter(
                and(List(
                  Predicates.observation.existence.includeDeleted(false),
                  Predicates.observation.program.isVisibleTo(user),
                  WHERE.getOrElse(True)
                )),
                child
              )
            )
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

  private lazy val ProgramNote: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    case (QueryType, "programNote", List(
      ProgramNoteIdBinding("programNoteId", rNid)
    )) =>
      Elab.transformChild: child =>
        rNid.map: nid =>
          Unique(
            Filter(And(Predicates.programNote.id.eql(nid), Predicates.programNote.isVisibleTo(user)), child)
          )
  }

  private lazy val ProgramNotes: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
    val WhereProgramNoteBinding = WhereProgramNote.binding(Path.from(ProgramNoteType))
    {
      case (QueryType, "programNotes", List(
        WhereProgramNoteBinding.Option("WHERE", rWHERE),
        ProgramNoteIdBinding.Option("OFFSET", rOFFSET),
        NonNegIntBinding.Option("LIMIT", rLIMIT),
        BooleanBinding("includeDeleted", rIncludeDeleted)
      )) =>
        Elab.transformChild: child =>
          (rWHERE, rOFFSET, rLIMIT, rIncludeDeleted).parTupled.flatMap: (WHERE, OFFSET, LIMIT, includeDeleted) =>
            val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
            ResultMapping.selectResult(child, limit): q =>
              FilterOrderByOffsetLimit(
                pred = Some(
                  and(List(
                    OFFSET.map(Predicates.programNote.id.gtEql).getOrElse(True),
                    Predicates.programNote.existence.includeDeleted(includeDeleted),
                    Predicates.programNote.isVisibleTo(user),
                    WHERE.getOrElse(True)
                  ))
                ),
                oss = Some(List(
                  OrderSelection[lucuma.core.model.ProgramNote.Id](ProgramNoteType / "id")
                )),
                offset = None,
                limit = Some(limit + 1), // Select one extra row here.
                child = q
              )
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
                  OrderSelection[Option[lucuma.core.model.User.Id]](ProgramUserType / "userId"),
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
            OrderBy(
              OrderSelections(List(
                OrderSelection[Instrument](SpectroscopyConfigOptionType / "instrument"),
                OrderSelection[NonEmptyString](SpectroscopyConfigOptionType / "name")
              )),
              Filter(where.getOrElse(True), child)
            )
          }
        }
    }
  }

  private lazy val ImagingConfigOptions: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    val WhereOptions = WhereImagingConfigOption.binding(Path.from(ImagingConfigOptionType))
    {
      case (QueryType, "imagingConfigOptions", List(
        WhereOptions.Option("WHERE", rWHERE)
      )) =>
        Elab.transformChild { child =>
          rWHERE.map { where =>
            OrderBy(
              OrderSelections(List(
                OrderSelection[Instrument](ImagingConfigOptionType / "instrument"),
                OrderSelection[String](ImagingConfigOptionType / "filterLabel"),
              )),
              Filter(where.getOrElse(True), child)
            )
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
