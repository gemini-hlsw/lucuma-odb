// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service
package workflow

import cats.*
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.implicits.*
import grackle.Result
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.DeclaredExecutionState
import lucuma.core.enums.DeclaredExecutionState.given
import lucuma.core.enums.ExecutionState as CoreExecutionState
import lucuma.core.enums.Instrument
import lucuma.core.enums.KeckInstrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.Observatory
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ProposalStatus
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.enums.SubaruInstrument
import lucuma.core.enums.TooActivation
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.model.CallCoordinatesLimits
import lucuma.core.model.CallForProposals
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SiteCoordinatesLimits
import lucuma.core.model.StandardRole.*
import lucuma.core.model.Target
import lucuma.core.util.DateInterval
import lucuma.core.util.Timestamp
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.service.ObservationWorkflowService.UserState
import lucuma.odb.syntax.instrument.*
import lucuma.odb.syntax.observingModeType.*
import lucuma.odb.util.Codecs.*
import skunk.Encoder
import skunk.Query
import skunk.Transaction
import skunk.syntax.all.*

import java.time.Instant

import Services.Syntax.*

/* Validation Info Record */
case class ObservationValidationInfo(
  pid:                    Program.Id,
  tpe:                    ProgramType,
  oid:                    Observation.Id,
  constraintSet:          ConstraintSet,
  spectroscopyWavelength: Option[Wavelength],
  observingMode:          Option[ObservingModeType],
  coordinates:            Option[Coordinates],  // explicit base, or coordinates at CFP midpoint, if any
  explicitBase:           Option[Coordinates],
  calibrationRole:        Option[CalibrationRole],
  userState:              Option[ObservationWorkflowService.UserState],
  declaredExecutionState: Option[DeclaredExecutionState],
  proposalStatus:         ProposalStatus,
  tooActivation:          TooActivation,
  tooCeiling:             Option[TooActivation], // effective proposal ceiling; None when the program has no proposal
  cfpid:                  Option[CallForProposals.Id],
  scienceBand:            Option[ScienceBand],
  asterism:               List[Target],
  associatedUserState:    Option[ObservationWorkflowService.UserState], // state of science obs if this is a per-observation calibration (telluric or daytime pinhole)
  generatorParams:        Option[Either[GeneratorParamsService.Error, GeneratorParams]] = None,
  cfpInfo:                Option[CfpInfo] = None,
  programAllocations:     Option[NonEmptyList[ScienceBand]] = None,
  otherConfigErrors:      List[String] = Nil,
  keckInstrument:         Option[KeckInstrument] = None,   // set for exchange_keck observations
  subaruInstrument:       Option[SubaruInstrument] = None, // set for exchange_subaru observations
) {

  def isDeclaredComplete: Boolean =
    declaredExecutionState === Some(CoreExecutionState.DeclaredComplete)

  def instrument: Option[Instrument] =
    observingMode.flatMap(_.instrumentOption)

  def effectiveUserState: Option[UserState] =
    // Per-observation calibrations (tellurics, daytime pinhole flats) inherit
    // their science observation's user state.
    // A telluric could be declined however, thus it can carrie its own Inactive state,
    // which overrides that inheritance.
    if calibrationRole.contains(CalibrationRole.Telluric) && userState.contains(ObservationWorkflowState.Inactive) then Some(ObservationWorkflowState.Inactive)
    else if calibrationRole.exists(ObsExtract.PerObservationCalibrationRoles.contains) then associatedUserState
    else userState

  /* Has the proposal been accepted? */
  def isAccepted:Boolean =
    proposalStatus === ProposalStatus.Accepted

  /**
   * Does this observation demand more Target-of-Opportunity disruption than the
   * program's proposal allows?  Before acceptance the ceiling is derived as the
   * maximum over the program's own observations, so this can only be true then
   * if the PI explicitly chose a ceiling below one of their observations -- also
   * worth surfacing.  A program with no proposal has no ceiling to enforce.
   */
  def exceedsTooCeiling: Boolean =
    tooCeiling.exists(tooActivation > _)

  def site: Option[Site] =
    instrument.map(_.site)

  /**
   * Is this a Target-of-Opportunity observation -- one that waits for an alert
   * rather than for the queue?  Setting such an observation `Ready` is what
   * requests its trigger.
   *
   * Independent of [[hasTooTarget]]: this is about the observation's declared
   * urgency, not about whether its target has been found yet.
   */
  def isTooObservation: Boolean =
    tooActivation =!= TooActivation.None

  /**
   * Does the asterism still hold an opportunity target -- the placeholder that
   * stands in for a Target of Opportunity until the real one is identified?
   *
   * Independent of [[isTooObservation]]: a placeholder is only coherent in an
   * observation that declares a ToO activation, and only until the alert
   * arrives, but neither implies the other.
   */
  def hasTooTarget: Boolean =
    asterism.exists:
      case t: Target.Opportunity => true
      case _ => false

  def isVisitor: Boolean =
    observingMode.exists:
      case _: VisitorObservingModeType => true
      case _ => false

  def isExchange: Boolean =
    observingMode.exists(_.isExchange)

  lazy val cfpMidpoint: Option[Timestamp] =
    for
      s  <- site
      c  <- cfpInfo
      ts <- Timestamp.fromInstant(c.midpoint(s))
    yield ts

}

case class CfpInfo(
  cfpid:             CallForProposals.Id,
  observatory:       Observatory,
  limits:            CallCoordinatesLimits,
  active:            DateInterval,
  instruments:       List[Instrument],        // Gemini instruments allowed by the call
  keckInstruments:   List[KeckInstrument],    // Keck exchange instruments allowed by the call
  subaruInstruments: List[SubaruInstrument]   // Subaru exchange instruments allowed by the call
) {

  def midpoint(at: Site): Instant =
    at.midpoint(active)

  def addInstrument(insts: Option[Instrument]): CfpInfo =
    insts.fold(this)(inst => copy(instruments = inst :: instruments))

}

object ObservationValidationInfo {

  def fetch[F[_]: Concurrent](
    oids: List[Observation.Id]
  )(using Transaction[F], Services[F]): F[Map[Observation.Id, ObservationValidationInfo]] = {

    def partialObsInfos(oids: NonEmptyList[Observation.Id]): F[Map[Observation.Id, ObservationValidationInfo]] =
      val enc = observation_id.nel(oids)
      session
        .stream(Statements.ObservationValidationInfosWithoutAsterisms(enc))(oids, 1024)
        .compile
        .toList
        .map: list =>
          list
            .fproductLeft(_.oid)
            .toMap

    def addAsterisms(input: Map[Observation.Id, ObservationValidationInfo]): F[Map[Observation.Id, ObservationValidationInfo]] =
      Services.asSuperUser:
        asterismService
          .getAsterisms(input.keys.toList)
          .map: results =>
            input.map: (oid, info) =>
              oid -> info.copy(asterism = results.get(oid).foldMap(_.map(_._2)))

    def addGeneratorParams(input: Map[Observation.Id, ObservationValidationInfo]): F[Map[Observation.Id, ObservationValidationInfo]] =
      generatorParamsService
        .selectMany(input.keys.toList)
        .map: results =>
          input.map: (oid, info) =>
            oid -> info.copy(generatorParams = results.get(oid))

    def addCfpInfos(input: Map[Observation.Id, ObservationValidationInfo]): F[Map[Observation.Id, ObservationValidationInfo]] =
      NonEmptyList.fromList(input.values.flatMap(_.cfpid).toList.distinct) match
        case None => input.pure[F]
        case Some(nel) =>
          val enc = cfp_id.nel(nel)
          session
            .stream(Statements.CfpInfos(enc))(nel, 1024)
            .compile
            .fold(Map.empty[CallForProposals.Id, CfpInfo]):
              case (m, (cfp, oinst)) =>
                m.updatedWith(cfp.cfpid):
                  case None    => cfp.addInstrument(oinst).some
                  case Some(c) => c.addInstrument(oinst).some
            .map: results =>
              input.map: (oid, info) =>
                oid -> info.copy(cfpInfo = info.cfpid.flatMap(results.get))

    // Enriches exchange observations with their chosen Keck/Subaru instrument.
    // Only exchange-mode observations are looked up, so non-exchange programs
    // pay nothing here.
    def addExchangeInfos(input: Map[Observation.Id, ObservationValidationInfo]): F[Map[Observation.Id, ObservationValidationInfo]] =
      val exchangeOids =
        input.collect:
          case (oid, info) if info.observingMode.exists(_.isExchange) => oid
        .toList
      if exchangeOids.isEmpty then input.pure[F]
      else
        exchangeService.select(exchangeOids).map: configs =>
          input.map: (oid, info) =>
            configs.get(oid).fold(oid -> info): cfg =>
              oid -> info.copy(keckInstrument = cfg.keckInstrument, subaruInstrument = cfg.subaruInstrument)

    def addProgramAllocations(input: Map[Observation.Id, ObservationValidationInfo]): F[Map[Observation.Id, ObservationValidationInfo]] =
      NonEmptyList.fromList(input.values.map(_.pid).toList.distinct) match
        case None => input.pure[F]
        case Some(nel) =>
          val enc = program_id.nel(nel)
          session
            .stream(Statements.ProgramAllocations(enc))(nel, 1024)
            .compile
            .toList
            .map: list =>
              list.foldMap: (pid, band) =>
                Map(pid -> NonEmptyList.one(band))
            .map: result =>
              input.map: (oid, info) =>
                oid -> info.copy(programAllocations = result.get(info.pid))

    def addCoordinates(input: Map[Observation.Id, ObservationValidationInfo]): F[Map[Observation.Id, ObservationValidationInfo]] =
      input
        .values
        .groupBy(_.cfpMidpoint)
        .collect:
          case (Some(k), v) => k -> v.toList.map(_.oid)
        .toList // we now have a list of (Timestamp, List[oid]) batches
        .foldLeftM(input): // fold over this list with `input` as the starting point, updating when we can find coordinates
          case (accum, (when, batch)) =>
            trackingService
              .getCoordinatesSnapshotOrRegion(batch, when, false)
              .map: batchResults =>
                batchResults
                  .collect:
                    case (oid, Result.Success(Left(snap))) => oid -> snap.base // we only care about results where coordinates are known
                    case (oid, Result.Success(Right(_, Some(eb)))) => oid -> eb
                  .foldLeft(accum):
                    case (accum2, (oid, coords)) =>
                      accum2.updatedWith(oid): op =>
                        op.map(_.copy(coordinates = Some(coords)))

    // Configuration errors that don't fit elsewhere.
    def addOtherConfigErrors(input: Map[Observation.Id, ObservationValidationInfo]): F[Map[Observation.Id, ObservationValidationInfo]] =
      import lucuma.odb.sequence.ghost.ifu.Config as GhostIfu

      extension (e: CoreExecutionState)
        def shouldCheck: Boolean =
          e match
            case CoreExecutionState.NotDefined |
                  CoreExecutionState.NotStarted => true
            case _                             => false

      // Get a list of GHOST observations that need to be checked.
      val ghostObsList: List[Observation.Id] =
        input
          .toList
          .mapFilter: (oid, info) =>
            info
              .generatorParams
              .flatMap(_.toOption)
              .flatMap: params =>
                params.observingMode match
                  case GhostIfu(_, _, _, _, _, _, _, _) if params.executionState.shouldCheck => oid.some
                  case _                                                                     => none

      // Validate each one to ensure it has an IFU mapping.
      ghostIfuService
        .validationErrors(ghostObsList)
        .map: errors =>
          errors.toList.foldLeft(input) { case (m, (oid, msg)) =>
            m.updatedWith(oid): info =>
              info.map(in => in.copy(otherConfigErrors = msg :: in.otherConfigErrors))
          }

    NonEmptyList.fromList(oids) match
      case None      =>
        Map.empty.pure
      case Some(nel) =>
        partialObsInfos(nel)
          .flatMap(addAsterisms)
          .flatMap(addGeneratorParams)
          .flatMap(addCfpInfos)
          .flatMap(addExchangeInfos)
          .flatMap(addProgramAllocations)
          .flatMap(addCoordinates)
          .flatMap(addOtherConfigErrors)
  }

  object Statements {

    def ObservationValidationInfosWithoutAsterisms[A <: NonEmptyList[Observation.Id]](enc: Encoder[A]): Query[A, ObservationValidationInfo] =
      sql"""
        SELECT
          o.c_program_id,
          p.c_program_type,
          o.c_observation_id,
          o.c_observing_mode_type,
          o.c_explicit_ra,
          o.c_explicit_dec,
          o.c_calibration_role,
          o.c_workflow_user_state,
          o.c_declared_state,
          p.c_proposal_status,
          o.c_too_activation,
          x.c_too_activation_effective,
          x.c_cfp_id,
          o.c_science_band,
          s.c_workflow_user_state,

          -- conditions
          o.c_cloud_extinction,
          o.c_image_quality,   
          o.c_sky_background,  
          o.c_water_vapor,

          -- relative order is important here; we're decoding a 4-col vector for elevationrange
          o.c_air_mass_min,    
          o.c_air_mass_max,    
          o.c_hour_angle_min,  
          o.c_hour_angle_max,

          o.c_spec_wavelength

        FROM t_observation o
        JOIN t_program p on p.c_program_id = o.c_program_id
        -- v_proposal rather than t_proposal: it adds the effective ToO ceiling
        -- (explicit, else derived from the program's observations).
        LEFT JOIN v_proposal x
          ON o.c_program_id = x.c_program_id
        LEFT JOIN t_observation s
          ON  o.c_calibration_role = ANY(ARRAY['telluric','daytime_pinhole']::e_calibration_role[])
          AND s.c_calibration_role IS NULL
          AND o.c_group_id = s.c_group_id
        WHERE o.c_observation_id IN ($enc)
      """
      .query(
        program_id                   *:
        program_type                 *:
        observation_id               *:
        observing_mode_type.opt      *:
        right_ascension.opt          *:
        declination.opt              *:
        calibration_role.opt         *:
        user_state.opt               *:
        declared_execution_state.opt *:
        proposal_status              *:
        too_activation               *:
        too_activation.opt           *:
        cfp_id.opt                   *:
        science_band.opt             *:
        user_state.opt               *:
        cloud_extinction_preset      *:
        image_quality_preset         *:
        sky_background               *:
        water_vapor                  *:
        elevation_range              *:
        wavelength_pm.opt
      )
      .map:
        case (pid, tpe, oid, mode, ra, dec, cal, state, ds, ps, too, ceil, cfp, sci, state2, ce, iq, sb, wv, er, wl) =>
          val cs = ConstraintSet(iq, ce, sb, wv, er)
          ObservationValidationInfo(pid, tpe, oid, cs, wl, mode, None, (ra, dec).mapN(Coordinates.apply), cal, state, ds, ps, too, ceil, cfp, sci, Nil, state2)

    def ProgramAllocations[A <: NonEmptyList[Program.Id]](enc: Encoder[A]): Query[A, (Program.Id, ScienceBand)] =
      sql"""
        SELECT DISTINCT
          c_program_id,
          c_science_band
        FROM
          t_allocation
        WHERE
          c_program_id IN ($enc)
      """.query(program_id *: science_band)

    def CfpInfos[A <: NonEmptyList[CallForProposals.Id]](enc: Encoder[A]): Query[A, (CfpInfo, Option[Instrument])] =
      sql"""
        SELECT
          c.c_cfp_id,
          c.c_observatory,
          c.c_north_ra_start,
          c.c_north_ra_end,
          c.c_north_dec_start,
          c.c_north_dec_end,
          c.c_south_ra_start,
          c.c_south_ra_end,
          c.c_south_dec_start,
          c.c_south_dec_end,
          c.c_active_start,
          c.c_active_end,
          c.c_keck_instruments,
          c.c_subaru_instruments,
          i.c_instrument
        FROM t_cfp c
        LEFT JOIN t_gemini_cfp_instrument i
        ON c.c_cfp_id = i.c_cfp_id
        WHERE c.c_cfp_id in ($enc)
      """.query(
          cfp_id                 *:
          observatory            *:
          right_ascension        *: // north limits
          right_ascension        *: // north limits
          declination            *: // north limits
          declination            *: // north liits
          right_ascension.opt    *: // south limits
          right_ascension.opt    *: // south limits
          declination.opt        *: // south limits
          declination.opt        *: // south limits
          date_interval          *: // active period
          _keck_instrument.opt   *:
          _subaru_instrument.opt *:
          instrument.opt
        ).map:
          // The south coordinate limits are null for exchange (keck/subaru) calls,
          // which use only the northern limits.  Exchange observations have no site
          // and so never exercise the coordinate-limit validation; we fall back to
          // the north limits to keep CallCoordinatesLimits total.
          case (id, obs, n_ra_s, n_ra_e, n_dec_s, n_dec_e, s_ra_s, s_ra_e, s_dec_s, s_dec_e, active, keck, subaru, oinst) =>
            val north = SiteCoordinatesLimits(n_ra_s, n_ra_e, n_dec_s, n_dec_e)
            val south = (s_ra_s, s_ra_e, s_dec_s, s_dec_e).mapN(SiteCoordinatesLimits.apply).getOrElse(north)
            (CfpInfo(id, obs, CallCoordinatesLimits(north, south), active, Nil, keck.orEmpty, subaru.orEmpty), oinst)

  }

}