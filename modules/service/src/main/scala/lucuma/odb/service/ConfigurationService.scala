// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Monoid
import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import io.circe.ACursor
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Coordinates
import lucuma.core.model.Configuration
import lucuma.core.model.Configuration.Conditions
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.asFailure
import lucuma.odb.data.OdbErrorExtensions.asWarning
import lucuma.odb.graphql.input.ConfigurationRequestPropertiesInput
import lucuma.odb.json.configurationrequest.query.DecodingFailures
import lucuma.odb.json.configurationrequest.query.given
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.AppliedFragment
import skunk.Query
import skunk.Transaction
import skunk.syntax.all.*

import Services.Syntax.*

trait ConfigurationService[F[_]] {

  /** Selects all configuration requests that subsume this observation's configuration. */
  def selectRequests(oid: Observation.Id)(using Transaction[F]): F[Result[List[ConfigurationRequest]]]

  /** Inserts (or selects) a `ConfigurationRequest` based on the configuration of `oid`. */
  def canonicalizeRequest(oid: Observation.Id)(using Transaction[F]): F[Result[ConfigurationRequest]]

  /** Creates`ConfigurationRequest`s as needed to ensure that one exists for each observation in `pid`. */
  def canonicalizeAll(pid: Program.Id)(using Transaction[F]): F[Result[Map[Observation.Id, ConfigurationRequest]]]

  /** Deletes all `ConfigurationRequest`s for `pid`, returning the ids of deleted configurations. */
  def deleteAll(pid: Program.Id)(using Transaction[F]): F[Result[List[ConfigurationRequest.Id]]]

  /** 
   * Updates the requests specified by `where`, which is an `AppliedFragment` that should return a stream
   * of request ids filtered to those where the caller has write access.
   */
  def updateRequests(SET: ConfigurationRequestPropertiesInput, where: AppliedFragment): F[Result[List[ConfigurationRequest.Id]]]

}

object ConfigurationService {

  extension (hc: ACursor) def downFields(fields: String*): ACursor = 
    fields.foldLeft(hc)(_.downField(_))

  def instantiate[F[_]: Concurrent](using Services[F]): ConfigurationService[F] =
    new ConfigurationService[F] {
      val impl = Impl[F]

      override def selectRequests(oid: Observation.Id)(using Transaction[F]): F[Result[List[ConfigurationRequest]]] =
        impl.selectRequests(oid).value

      override def canonicalizeRequest(oid: Observation.Id)(using Transaction[F]): F[Result[ConfigurationRequest]] =
        impl.canonicalizeRequest(oid).value

      override def canonicalizeAll(pid: Program.Id)(using Transaction[F]): F[Result[Map[Observation.Id, ConfigurationRequest]]] =
        impl.canonicalizeAll(pid).value

      override def deleteAll(pid: Program.Id)(using Transaction[F]): F[Result[List[ConfigurationRequest.Id]]] =
        session.prepareR(Statements.DeleteRequests).use: pq =>
          pq.stream(pid, 1024).compile.toList.map(Result(_))

      override def updateRequests(SET: ConfigurationRequestPropertiesInput, where: AppliedFragment): F[Result[List[ConfigurationRequest.Id]]] =        
        val doUpdate = impl.updateRequests(SET, where).value
        SET.status match
          case None                                       | 
               Some(ConfigurationRequestStatus.Requested) | 
               Some(ConfigurationRequestStatus.Withdrawn) => requirePiAccess(doUpdate)
          case Some(ConfigurationRequestStatus.Approved)  |
               Some(ConfigurationRequestStatus.Denied)    => requireStaffAccess(doUpdate)                  

    }

  /** An implementation with unwrapped parameters and results in more natural types. */
  private class Impl[F[_]: Concurrent](using Services[F]) {

    private def selectConfiguration(oid: Observation.Id)(using Transaction[F]): ResultT[F, Configuration] =
      ResultT:
        selectConfigurations(List(oid)).value.map: result =>
          result.flatMap: map =>
            map.get(oid) match
              case Some(config) => Result(config)          
              case None => OdbError.InvalidConfiguration(Some(s"Observation $oid is invalid or has an incomplete configuration.")).asFailure

    // A monoid specifically for the fold below, which concatenates maps
    private given Monoid[Result[Map[Observation.Id, Configuration]]] =
      Monoid.instance(Result(Map.empty), (a, b) => (a, b).mapN(_ ++ _))

    /** Select the configurations for many observations, warning for any that are invalid. */
    private def selectConfigurationsImpl(graphQLQuery: String)(using Transaction[F]): F[Result[Map[Observation.Id, Configuration]]] =
      services.runGraphQLQuery(graphQLQuery).map: r =>
        r.flatMap: json =>
          json.hcursor.downField("observations").downField("matches").as[List[Json]] match
            case Left(error)  => Result.failure(error.getMessage) // Should never happen
            case Right(jsons) =>
              jsons.foldMap: json =>
                val hc = json.hcursor
                hc.downField("id").as[Observation.Id] match
                  case Left(value) => Result.internalError(value.getMessage) 
                  case Right(obsid) =>                       
                    hc.downField("configuration").as[Configuration] match
                      case Right(cfg) => Result(Map(obsid -> cfg))
                      case Left(DecodingFailures.NoReferenceCoordinates) => OdbError.InvalidConfiguration(Some(s"Reference coordinates are not available for observation $obsid.")).asWarning(Map.empty)
                      case Left(DecodingFailures.NoObservingMode)        => OdbError.InvalidConfiguration(Some(s"Observing mode is undefined for observation $obsid.")).asWarning(Map.empty)
                      case Left(other) => Result.internalError(other.getMessage)

    private def selectConfigurations(oids: List[Observation.Id])(using Transaction[F]): ResultT[F, Map[Observation.Id, Configuration]] =
      ResultT:
        selectConfigurationsImpl(Queries.selectConfigurations(oids)).map: res =>
          oids.foldLeft(res): (res, oid) =>
            if res.toOption.exists(_.contains(oid)) then res
            else res |+| OdbError.InvalidConfiguration(Some(s"Observation $oid is invalid or has an incomplete configuration.")).asWarning(Map.empty)

    private def selectConfigurations(pid: Program.Id)(using Transaction[F]): ResultT[F, Map[Observation.Id, Configuration]] =
      ResultT(selectConfigurationsImpl(Queries.selectConfigurations(pid)))

    private def selectAllRequestsForProgram(oid: Observation.Id)(using Transaction[F]): ResultT[F, List[ConfigurationRequest]] =
      ResultT:
        services.runGraphQLQuery(Queries.selectAllRequestsForProgram(oid)).map: r =>
          r.flatMap: json =>
            json.hcursor.downFields("observation", "program", "configurationRequests", "matches").as[List[ConfigurationRequest]] match
              case Left(value)  => Result.failure(value.getMessage) // TODO: this probably isn't good enough
              case Right(value) => Result(value)

    private def canonicalizeRequest(oid: Observation.Id, cfg: Configuration)(using Transaction[F]): ResultT[F, ConfigurationRequest] =
      ResultT.liftF:
        session.prepareR(Statements.InsertRequest).use: pq =>
          pq.option(oid, cfg).flatMap:
            case Some(req) => req.pure[F]
            case None      =>
              session.prepareR(Statements.SelectRequest).use: pq =>
                pq.unique(oid, cfg)

    def selectRequests(oid: Observation.Id)(using Transaction[F]): ResultT[F, List[ConfigurationRequest]] =
      selectAllRequestsForProgram(oid).flatMap: crs =>
        if crs.isEmpty then Nil.pure[ResultT[F, *]] // in this case we can avoid the call to `selectConfiguration`
        else selectConfiguration(oid).map: cfg =>
          crs.filter(_.configuration.subsumes(cfg))

    def canonicalizeRequest(oid: Observation.Id)(using Transaction[F]): ResultT[F, ConfigurationRequest] = 
      selectConfiguration(oid).flatMap(canonicalizeRequest(oid, _))

    def canonicalizeAll(pid: Program.Id)(using Transaction[F]): ResultT[F, Map[Observation.Id, ConfigurationRequest]] =
      selectConfigurations(pid).flatMap: map =>
        map.toList.traverse((oid, config) => canonicalizeRequest(oid, config).tupleLeft(oid)).map(_.toMap)

    def updateRequests(SET: ConfigurationRequestPropertiesInput, where: AppliedFragment): ResultT[F, List[ConfigurationRequest.Id]] =
      // access level has been checked already
      ResultT.liftF:
        val af = Statements.updateRequests(SET, where)
        session.prepareR(af.fragment.query(configuration_request_id)).use: pq =>
          pq.stream(af.argument, 1024).compile.toList

  } 

  private object Queries {

    def selectConfigurations(oids: List[Observation.Id]): String =
      selectConfigurations(whereOidsIn(oids))

    def selectConfigurations(pid: Program.Id): String =
      selectConfigurations(wherePid(pid))

    private def whereOidsIn(oids: List[Observation.Id]) =
      s"""
        WHERE: {
          id: {
            IN: ${oids.asJson}
          }
        }
      """

    private def wherePid(pid: Program.Id) =
      s"""
        WHERE: {
          program: {
            id: {
              EQ: ${pid.asJson}
            }
          }
        }
      """

    private def selectConfigurations(where: String): String =
      s"""
        query {
          observations(           
            $where
            LIMIT: 1000 # TODO: we need unlimited in this case
          ) {
            matches {
              id
              configuration {
                conditions {
                  imageQuality
                  cloudExtinction
                  skyBackground
                  waterVapor
                }
                referenceCoordinates {
                  ra { 
                    hms 
                  }
                  dec { 
                    dms 
                  }
                }
                observingMode {
                  instrument
                  mode
                  gmosNorthLongSlit {
                    grating
                  }
                  gmosSouthLongSlit {
                    grating
                  }
                }
              }
            }
          }
        }
      """
  
    def selectAllRequestsForProgram(oid: Observation.Id) =
      s"""
        query {
          observation(observationId: "$oid") {
            program {
              configurationRequests {
                matches {
                  id
                  status
                  configuration {
                    conditions {
                      imageQuality
                      cloudExtinction
                      skyBackground
                      waterVapor
                    }
                    referenceCoordinates {
                      ra { 
                        hms 
                      }
                      dec { 
                        dms 
                      }
                    }
                    observingMode {
                      instrument
                      mode
                      gmosNorthLongSlit {
                        grating
                      }
                      gmosSouthLongSlit {
                        grating
                      }
                    }
                  }
                }
              }
            }
          }
        }
      """

  }

  private object Statements {

    // select matching row, if any
    val SelectRequest: Query[(Observation.Id, Configuration), ConfigurationRequest] =
      sql"""
        SELECT
          c_configuration_request_id,
          c_status,
          c_cloud_extinction,
          c_image_quality,
          c_sky_background,
          c_water_vapor,
          c_reference_ra,
          c_reference_dec,
          c_observing_mode_type,
          c_gmos_north_longslit_grating,
          c_gmos_south_longslit_grating
        FROM t_configuration_request
        WHERE (
          c_program_id = (select c_program_id from t_observation where c_observation_id = $observation_id) AND
          c_cloud_extinction = $cloud_extinction AND
          c_image_quality = $image_quality AND
          c_sky_background = $sky_background AND
          c_water_vapor = $water_vapor AND
          c_reference_ra = $right_ascension AND
          c_reference_dec = $declination AND
          c_observing_mode_type = $observing_mode_type AND
          c_gmos_north_longslit_grating is not distinct from ${gmos_north_grating.opt} AND
          c_gmos_south_longslit_grating is not distinct from ${gmos_south_grating.opt}
        ) 
      """.query(
        (
          configuration_request_id *:
          configuration_request_status *:
          cloud_extinction         *:
          image_quality            *:
          sky_background           *:
          water_vapor              *:
          right_ascension          *:
          declination              *:
          observing_mode_type      *:
          gmos_north_grating.opt   *:
          gmos_south_grating.opt
        ).emap:       
          { case 
            id                       *:
            status                   *:
            cloudExtinction          *:
            imageQuality             *:
            skyBackground            *:
            waterVapor               *:
            rightAscension           *:
            declination              *:
            observingModeType        *:
            gmosNorthLongSlitGrating *:
            gmosSouthLongSlitGrating *:
            EmptyTuple =>

              val mode: Either[String, Configuration.ObservingMode] = 
                (observingModeType, gmosNorthLongSlitGrating, gmosSouthLongSlitGrating) match

                  case (ObservingModeType.GmosNorthLongSlit, Some(g), _) => 
                    Right(Configuration.ObservingMode.GmosNorthLongSlit(g))
                  
                  case (ObservingModeType.GmosSouthLongSlit, _, Some(g)) => 
                    Right(Configuration.ObservingMode.GmosSouthLongSlit(g))
                  
                  case _ => Left(s"Malformed observing mode for configuration request $configuration_request_id")

              mode.map: m =>
                ConfigurationRequest(
                  id, 
                  status,
                  Configuration(
                    Conditions(
                      cloudExtinction,
                      imageQuality,
                      skyBackground,
                      waterVapor
                    ),
                    Coordinates(
                      rightAscension,
                      declination
                    ),
                    m
                  )
                )

          }
      ).contramap[(Observation.Id, Configuration)] { (oid, cfg) => 
        oid                                                         *:
        cfg.conditions.cloudExtinction                              *:
        cfg.conditions.imageQuality                                 *:
        cfg.conditions.skyBackground                                *:
        cfg.conditions.waterVapor                                   *:
        cfg.refererenceCoordinates.ra                               *:
        cfg.refererenceCoordinates.dec                              *:
        cfg.observingMode.tpe                                       *:
        cfg.observingMode.gmosNorthLongSlit.map(_.grating) *:
        cfg.observingMode.gmosSouthLongSlit.map(_.grating) *:
        EmptyTuple
      }

    // insert and return row, or return nothing if a matching row exists
    val InsertRequest: Query[(Observation.Id, Configuration), ConfigurationRequest] =
      sql"""
        INSERT INTO t_configuration_request (
          c_program_id,
          c_cloud_extinction,
          c_image_quality,
          c_sky_background,
          c_water_vapor,
          c_reference_ra,
          c_reference_dec,
          c_observing_mode_type,
          c_gmos_north_longslit_grating,
          c_gmos_south_longslit_grating
        ) VALUES (
          (select c_program_id from t_observation where c_observation_id = $observation_id),
          $cloud_extinction,
          $image_quality,
          $sky_background,
          $water_vapor,
          $right_ascension,
          $declination,
          $observing_mode_type,
          ${gmos_north_grating.opt},
          ${gmos_south_grating.opt}
        ) 
        ON CONFLICT DO NOTHING
        RETURNING
          c_configuration_request_id,
          c_status,
          c_cloud_extinction,
          c_image_quality,
          c_sky_background,
          c_water_vapor,
          c_reference_ra,
          c_reference_dec,
          c_observing_mode_type,
          c_gmos_north_longslit_grating,
          c_gmos_south_longslit_grating
      """.query(
        (
          configuration_request_id *:
          configuration_request_status *:
          cloud_extinction         *:
          image_quality            *:
          sky_background           *:
          water_vapor              *:
          right_ascension          *:
          declination              *:
          observing_mode_type      *:
          gmos_north_grating.opt                  *:
          gmos_south_grating.opt
        ).emap:       
          { case 
            id                       *:
            status                   *:
            cloudExtinction          *:
            imageQuality             *:
            skyBackground            *:
            waterVapor               *:
            rightAscension           *:
            declination              *:
            observingModeType        *:
            gmosNorthLongSlitGrating *:
            gmosSouthLongSlitGrating *:
            EmptyTuple =>

              val mode: Either[String, Configuration.ObservingMode] = 
                (observingModeType, gmosNorthLongSlitGrating, gmosSouthLongSlitGrating) match

                  case (ObservingModeType.GmosNorthLongSlit, Some(g), _) => 
                    Right(Configuration.ObservingMode.GmosNorthLongSlit(g))
                  
                  case (ObservingModeType.GmosSouthLongSlit, _, Some(g)) => 
                    Right(Configuration.ObservingMode.GmosSouthLongSlit(g))
                  
                  case _ => Left(s"Malformed observing mode for configuration request $configuration_request_id")

              mode.map: m =>
                ConfigurationRequest(
                  id, 
                  status,
                  Configuration(
                    Conditions(
                      cloudExtinction,
                      imageQuality,
                      skyBackground,
                      waterVapor
                    ),
                    Coordinates(
                      rightAscension,
                      declination
                    ),
                    m
                  )
                )

          }
      ).contramap[(Observation.Id, Configuration)] { (oid, cfg) => 
        oid                                                         *:
        cfg.conditions.cloudExtinction                              *:
        cfg.conditions.imageQuality                                 *:
        cfg.conditions.skyBackground                                *:
        cfg.conditions.waterVapor                                   *:
        cfg.refererenceCoordinates.ra                               *:
        cfg.refererenceCoordinates.dec                              *:
        cfg.observingMode.tpe                                       *:
        cfg.observingMode.gmosNorthLongSlit.map(_.grating) *:
        cfg.observingMode.gmosSouthLongSlit.map(_.grating) *:
        EmptyTuple
      }

    val DeleteRequests: Query[Program.Id, ConfigurationRequest.Id] =
      // todo: access control
      sql"""
        delete from t_configuration_request
        where c_program_id = $program_id
        returning c_configuration_request_id
      """.query(configuration_request_id)

    // applied fragment yielding a stream of ConfigurationRequest.Id
    def updateRequests(SET: ConfigurationRequestPropertiesInput, which: AppliedFragment): AppliedFragment =
      val statusExpr: AppliedFragment = SET.status.fold(void"c_status")(sql"$configuration_request_status".apply)
      void"""
        update t_configuration_request
        set c_status = """ |+| statusExpr |+| 
      void" where c_configuration_request_id in (" |+| which |+| void""")
        returning c_configuration_request_id
      """
  }

}

