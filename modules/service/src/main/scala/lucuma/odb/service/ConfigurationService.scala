// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import io.circe.ACursor
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.math.Coordinates
import lucuma.core.model.Observation
import lucuma.odb.data.Configuration
import lucuma.odb.data.Configuration.Conditions
import lucuma.odb.data.ConfigurationRequest
import lucuma.odb.data.ObservingModeType
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.asFailure
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.Query
import skunk.Transaction
import skunk.syntax.all.*

import Services.Syntax.*

trait ConfigurationService[F[_]] {

  /** Selects all configuration requests that subsume this observation's configuration. */
  def selectRequests(oid: Observation.Id)(using Transaction[F]): F[Result[List[ConfigurationRequest]]]

  /* Inserts a new `ConfigurationRequest` based on the configuration of `oid`. */
  def insertRequest(oid: Observation.Id)(using Transaction[F]): F[Result[ConfigurationRequest]]

}

object ConfigurationService {

  extension (hc: ACursor) def downFields(fields: String*): ACursor = 
    fields.foldLeft(hc)(_.downField(_))

  def instantiate[F[_]: Concurrent](using Services[F]): ConfigurationService[F] =
    new ConfigurationService[F] {
      val impl = Impl[F]

      override def selectRequests(oid: Observation.Id)(using Transaction[F]): F[Result[List[ConfigurationRequest]]] =
        impl.selectRequests(oid).value

      override def insertRequest(oid: Observation.Id)(using Transaction[F]): F[Result[ConfigurationRequest]] =
        impl.insertRequest(oid).value

    }

  /** An implementation with unwrapped parameters and results in more natural types. */
  private class Impl[F[_]: Concurrent](using Services[F]) {

    def selectConfiguration(oid: Observation.Id)(using Transaction[F]): ResultT[F, Configuration] =
      ResultT:
        services.runGraphQLQuery(Queries.selectConfiguration(oid)).map: r =>
          r.flatMap: json =>
            json.hcursor.downField("observation").downField("configuration").as[Configuration] match
              case Right(conf) => Result(conf)
              case Left(Configuration.DecodingFailures.NoReferenceCoordinates) => OdbError.InvalidConfiguration(Some("Reference coordinates are not available.")).asFailure
              case Left(Configuration.DecodingFailures.NoObservingMode) => OdbError.InvalidConfiguration(Some("Observing mode is undefined.")).asFailure
              case Left(other)  => Result.failure(other.getMessage) // TODO: this probably isn't good enough

    def selectAllRequestsForProgram(oid: Observation.Id)(using Transaction[F]): ResultT[F, List[ConfigurationRequest]] =
      ResultT:
        services.runGraphQLQuery(Queries.selectAllRequestsForProgram(oid)).map: r =>
          r.flatMap: json =>
            json.hcursor.downFields("observation", "program", "configurationRequests", "matches").as[List[ConfigurationRequest]] match
              case Left(value)  => Result.failure(value.getMessage) // TODO: this probably isn't good enough
              case Right(value) => Result(value)

    def selectRequests(oid: Observation.Id)(using Transaction[F]): ResultT[F, List[ConfigurationRequest]] =
      selectAllRequestsForProgram(oid).flatMap: crs =>
        if crs.isEmpty then Nil.pure[ResultT[F, *]] // in this case we can avoid the call to `selectConfiguration`
        else selectConfiguration(oid).map: cfg =>
          crs.filter(_.configuration.subsumes(cfg))

    def insertRequest(oid: Observation.Id)(using Transaction[F]): ResultT[F, ConfigurationRequest] = 
      selectConfiguration(oid).flatMap(insertRequest(oid, _))

    def insertRequest(oid: Observation.Id, cfg: Configuration)(using Transaction[F]): ResultT[F, ConfigurationRequest] =
      ResultT.liftF:
        session.prepareR(Statements.InsertRequest).use: pq =>
          pq.unique(oid, cfg)

  } 

  private object Queries {

    def selectConfiguration(oid: Observation.Id) =
      s"""
        query {
          observation(observationId: "$oid") {
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
        ) RETURNING
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

  }

}

