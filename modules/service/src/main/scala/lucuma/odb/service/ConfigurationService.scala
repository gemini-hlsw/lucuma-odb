// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import lucuma.core.model.Observation
import lucuma.odb.data.Configuration
import lucuma.odb.data.ConfigurationRequest
import lucuma.odb.service.ExecutionEventService.liftF
import skunk.Query
import skunk.Transaction

import Services.Syntax.*
import io.circe.ACursor

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
              case Left(value)  => Result.failure(value.getMessage) // TODO: this probably isn't good enough
              case Right(value) => Result(value)

    def selectAllRequestsForProgram(oid: Observation.Id)(using Transaction[F]): ResultT[F, List[ConfigurationRequest]] =
      ResultT:
        services.runGraphQLQuery(Queries.selectConfiguration(oid)).map: r =>
          r.flatMap: json =>
            json.hcursor.downFields("observation", "program", "configurationRequests").as[List[ConfigurationRequest]] match
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
  
    }

  def selectAllRequestsForProgram(oid: Observation.Id) =
    s"""
      query {
        observation(observationId: "$oid") {
          program {
            configurationRequests {
              id
              status
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

  private object Statements {

    val InsertRequest: Query[(Observation.Id, Configuration), ConfigurationRequest] =
      ???

  }

}

