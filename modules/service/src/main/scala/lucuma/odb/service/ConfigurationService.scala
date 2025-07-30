// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Monoid
import cats.effect.Concurrent
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import io.circe.ACursor
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Coordinates
import lucuma.core.math.Region
import lucuma.core.model.Configuration
import lucuma.core.model.Configuration.Conditions
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.asFailure
import lucuma.odb.data.OdbErrorExtensions.asWarning
import lucuma.odb.graphql.input.ConfigurationRequestPropertiesInput
import lucuma.odb.graphql.input.CreateConfigurationRequestInput
import lucuma.odb.json.configurationrequest.query.DecodingFailures
import lucuma.odb.json.configurationrequest.query.given
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.AppliedFragment
import skunk.Query
import skunk.Transaction
import skunk.syntax.all.*

import lucuma.odb.json.arc.tag

import Services.Syntax.*
import lucuma.core.enums.ArcType
import lucuma.core.math.Arc
import lucuma.core.math.RightAscension
import lucuma.core.math.Declination
import lucuma.core.math.Angular

trait ConfigurationService[F[_]] {

  /** Selects all configuration requests that subsume this observation's configuration. */
  def selectRequests(oid: Observation.Id)(using Transaction[F]): F[Result[List[ConfigurationRequest]]]

  /** Selects configuration requests relevant to the given program + observation pairs. The resulting map will contain every passed key. */
  def selectRequests(pairs: List[(Program.Id, Observation.Id)]): F[Result[Map[(Program.Id, Observation.Id), List[ConfigurationRequest]]]]

  /** Selects observations relevant to the given configuration requests, if any. The resulting map will contain every passed request id. */
  def selectObservations(rids: List[ConfigurationRequest.Id]): F[Result[Map[ConfigurationRequest.Id, List[Observation.Id]]]]

  /** Inserts (or selects) a `ConfigurationRequest` based on the configuration of `oid`. */
  def canonicalizeRequest(input: CreateConfigurationRequestInput)(using Transaction[F]): F[Result[ConfigurationRequest]]

  /** Creates`ConfigurationRequest`s as needed to ensure that one exists for each non-inactive, non-calibration observation in `pid`. */
  def canonicalizeAll(pid: Program.Id)(using Transaction[F]): F[Result[Map[Observation.Id, ConfigurationRequest]]]

  /** Deletes all `ConfigurationRequest`s for `pid`, returning the ids of deleted configurations. */
  def deleteAll(pid: Program.Id)(using Transaction[F]): F[Result[List[ConfigurationRequest.Id]]]

  /** 
   * Updates the requests specified by `where`, which is an `AppliedFragment` that should return a stream
   * of request ids filtered to those where the caller has write access.
   */
  def updateRequests(SET: ConfigurationRequestPropertiesInput.Update, where: AppliedFragment): F[Result[List[ConfigurationRequest.Id]]]

}

object ConfigurationService {

  extension [A](self: Result[A]) def suppressWarnings: Result[A] =
    self match
      case Result.Warning(problems, value) => Result(value)
      case other => other
    
  extension (hc: ACursor) def downFields(fields: String*): ACursor = 
    fields.foldLeft(hc)(_.downField(_))

  def instantiate[F[_]: Concurrent](using Services[F]): ConfigurationService[F] =
    new ConfigurationService[F] {
      val impl = Impl[F]

      override def selectRequests(oid: Observation.Id)(using Transaction[F]): F[Result[List[ConfigurationRequest]]] =
        impl.selectRequests(oid).value

      override def canonicalizeRequest(input: CreateConfigurationRequestInput)(using Transaction[F]): F[Result[ConfigurationRequest]] =
        impl.canonicalizeRequest(input).value

      override def canonicalizeAll(pid: Program.Id)(using Transaction[F]): F[Result[Map[Observation.Id, ConfigurationRequest]]] =
        impl.canonicalizeAll(pid).value

      override def deleteAll(pid: Program.Id)(using Transaction[F]): F[Result[List[ConfigurationRequest.Id]]] =
        session.prepareR(Statements.DeleteRequests).use: pq =>
          pq.stream(pid, 1024).compile.toList.map(Result(_))

      override def updateRequests(SET: ConfigurationRequestPropertiesInput.Update, where: AppliedFragment): F[Result[List[ConfigurationRequest.Id]]] =        
        val doUpdate = impl.updateRequests(SET, where).value
        SET.status match
          case None                                       | 
               Some(ConfigurationRequestStatus.Requested) | 
               Some(ConfigurationRequestStatus.Withdrawn) => requirePiAccess(doUpdate)
          case Some(ConfigurationRequestStatus.Approved)  |
               Some(ConfigurationRequestStatus.Denied)    => requireStaffAccess(doUpdate)                  

      override def selectRequests(pairs: List[(Program.Id, Observation.Id)]): F[Result[Map[(Program.Id, Observation.Id), List[ConfigurationRequest]]]] =
        impl.selectRequests(pairs).value

      override def selectObservations(rids: List[ConfigurationRequest.Id]): F[Result[Map[ConfigurationRequest.Id, List[Observation.Id]]]] =
        impl.selectObservations(rids).value.map(_.suppressWarnings) // we can disregard warnings

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
    @annotation.nowarn("msg=unused implicit parameter")
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

    @annotation.nowarn("msg=unused implicit parameter")
    private def selectAllRequestsForProgram(oid: Observation.Id)(using Transaction[F]): ResultT[F, List[ConfigurationRequest]] =
      ResultT:
        services.runGraphQLQuery(Queries.selectAllRequestsForProgram(oid)).map: r =>
          r.flatMap: json =>
            json.hcursor.downFields("observation", "program", "configurationRequests", "matches").as[List[ConfigurationRequest]] match
              case Left(value)  => Result.failure(value.getMessage) // TODO: this probably isn't good enough
              case Right(value) => Result(value)
            
    private def queryRequestsAndConfigurations(
      pids: List[Program.Id], 
      oids: List[Observation.Id]
    ): ResultT[F, (Map[Program.Id, List[ConfigurationRequest]], Map[(Program.Id, Observation.Id), Configuration])] =
      if pids.isEmpty && oids.isEmpty then
        ResultT.pure((Map.empty[Program.Id, List[ConfigurationRequest]], Map.empty[(Program.Id, Observation.Id), Configuration]))
      else
        ResultT:
          services.runGraphQLQuery(Queries.RequestsAndConfigurations(pids, oids)).map: r =>
            r.flatMap: json =>
              import Queries.RequestsAndConfigurations.given
              json.hcursor.as[Queries.RequestsAndConfigurations.Response] match
                case Right(r)    => Result(r)
                case Left(other) => Result.internalError(other.getMessage)

    @annotation.nowarn("msg=unused implicit parameter")
    private def canonicalizeRequest(input: CreateConfigurationRequestInput, cfg: Configuration)(using Transaction[F]): ResultT[F, ConfigurationRequest] =
      ResultT.liftF:
        session.prepareR(Statements.InsertRequest).use: pq =>
          pq.option(input, cfg).flatMap:
            case Some(req) => req.pure[F]
            case None      =>
              session.prepareR(Statements.SelectRequest).use: pq =>
                pq.unique(input.oid, cfg)

    def selectRequests(oid: Observation.Id)(using Transaction[F]): ResultT[F, List[ConfigurationRequest]] =
      selectAllRequestsForProgram(oid).flatMap: crs =>
        if crs.isEmpty then Nil.pure[ResultT[F, *]] // in this case we can avoid the call to `selectConfiguration`
        else selectConfiguration(oid).map: cfg =>
          crs.filter(_.configuration.subsumes(cfg))

    def canonicalizeRequest(input: CreateConfigurationRequestInput)(using Transaction[F]): ResultT[F, ConfigurationRequest] = 
      selectConfiguration(input.oid).flatMap(canonicalizeRequest(input, _))

    def canonicalizeAll(pid: Program.Id)(using Transaction[F]): ResultT[F, Map[Observation.Id, ConfigurationRequest]] =
      ResultT
        .liftF:
          session.prepareR(Statements.SelectActiveNonCalibrations).use: pq =>
            pq.stream(pid, 1024).compile.toList
        .flatMap: oids =>
          selectConfigurations(oids).flatMap: map =>
            map.toList.traverse((oid, config) => canonicalizeRequest(CreateConfigurationRequestInput(oid), config).tupleLeft(oid)).map(_.toMap)

    def updateRequests(SET: ConfigurationRequestPropertiesInput.Update, where: AppliedFragment): ResultT[F, List[ConfigurationRequest.Id]] =
      // access level has been checked already
      ResultT.liftF:
        val af = Statements.updateRequests(SET, where)
        session.prepareR(af.fragment.query(configuration_request_id)).use: pq =>
          pq.stream(af.argument, 1024).compile.toList

    def selectRequests(pairs: List[(Program.Id, Observation.Id)]): ResultT[F, Map[(Program.Id, Observation.Id), List[ConfigurationRequest]]] =
      queryRequestsAndConfigurations(pairs.map(_._1).distinct, pairs.map(_._2).distinct).map: (pmap, omap) =>
        pairs
          .fproduct: key =>
            (omap.get(key), pmap.get(key._1))
              .tupled
              .foldMap: (cfg, nel) =>
                nel.filter(_.configuration.subsumes(cfg))
          .toMap

    def queryRequestsAndObservations(
      rids: List[ConfigurationRequest.Id]
    ): ResultT[F, List[(ConfigurationRequest, List[(Observation.Id, Configuration)])]] =
      if rids.isEmpty then
        ResultT.pure(List.empty)
      else
        ResultT:
          services.runGraphQLQuery(Queries.SelectRequestsAndObservations(rids)).map: r =>
            r.flatMap: json =>
              import Queries.SelectRequestsAndObservations.given
              json.hcursor.as[Queries.SelectRequestsAndObservations.Response] match
                case Right(r)    => Result(r)
                case Left(other) => Result.internalError(other.getMessage)

    def selectObservations(rids: List[ConfigurationRequest.Id]): ResultT[F, Map[ConfigurationRequest.Id, List[Observation.Id]]] =
      queryRequestsAndObservations(rids.distinct).map: result =>         
        result
          .map:
            case (req, list) =>
              req.id ->
                list.collect:
                  case (oid, cfg) if req.configuration.subsumes(cfg) => oid
          .toMap        
          .withDefaultValue(Nil)

  } 

  object Queries {

    object RequestsAndConfigurations:
      type Response = (Map[Program.Id, List[ConfigurationRequest]], Map[(Program.Id, Observation.Id), Configuration])

      private given Decoder[(Program.Id, List[ConfigurationRequest])] = hc =>
        for
          id  <- hc.downField("id").as[Program.Id]
          crs <- hc.downFields("configurationRequests", "matches").as[List[ConfigurationRequest]]
        yield (id, crs)

      private given Decoder[((Program.Id, Observation.Id), Option[Configuration])] = hc =>
        for
          pid <- hc.downFields("program", "id").as[Program.Id]
          oid <- hc.downField("id").as[Observation.Id]
          cfg  = hc.downField("configuration").as[Configuration].toOption
        yield ((pid, oid), cfg)

      given Decoder[Response] = hc =>
        for
          m1 <- hc.downFields("programs", "matches").as[List[(Program.Id, List[ConfigurationRequest])]]
          m2 <- hc.downFields("observations", "matches").as[List[((Program.Id, Observation.Id), Option[Configuration])]]
        yield (m1.toMap, m2.collect { case ((pid, oid), Some(cfg)) => ((pid, oid), cfg) }.toMap)

      def apply(
        pids: List[Program.Id], 
        oids: List[Observation.Id]
      ): String =
        s"""
          query {

            observations(           
              ${whereIdsIn(oids)}
            ) {
              matches {
                id
                program {
                  id
                }
                configuration {
                  conditions {
                    imageQuality
                    cloudExtinction
                    skyBackground
                    waterVapor
                  }
                  target {
                    coordinates {
                      ra { 
                        hms 
                      }
                      dec { 
                        dms 
                      }
                    }
                    region {
                      rightAscensionArc {
                        type
                        start { hms }
                        end { hms }
                      }
                      declinationArc {
                        type
                        start { dms }
                        end { dms }
                      }
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

            programs(
              ${whereIdsIn(pids)}
            ) {
              matches {
                id
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
                      target {
                        coordinates {
                          ra { 
                            hms 
                          }
                          dec { 
                            dms 
                          }
                        }
                        region {
                          rightAscensionArc {
                            type
                            start { hms }
                            end { hms }
                          }
                          declinationArc {
                            type
                            start { dms }
                            end { dms }
                          }
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

    def selectConfigurations(oids: List[Observation.Id]): String =
      selectConfigurations(whereIdsIn(oids))

    def selectConfigurations(pid: Program.Id): String =
      selectConfigurations(wherePid(pid))

    private def whereIdsIn[A: Encoder](ids: List[A]) =
      s"""
        WHERE: {
          id: {
            IN: ${ids.asJson}
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
                target {
                  coordinates {
                    ra { 
                      hms 
                    }
                    dec { 
                      dms 
                    }
                  }
                  region {
                    rightAscensionArc {
                      type
                      start { hms }
                      end { hms }
                    }
                    declinationArc {
                      type
                      start { dms }
                      end { dms }
                    }
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
                    target {
                      coordinates: {
                        ra { 
                          hms 
                        }
                        dec { 
                          dms 
                        }
                      }
                      region {
                        rightAscensionArc {
                          type
                          start { hms }
                          end { hms }
                        }
                        declinationArc {
                          type
                          start { dms }
                          end { dms }
                        }
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

    object SelectRequestsAndObservations:
      type Response = List[(ConfigurationRequest, List[(Observation.Id, Configuration)])]

      private given dc: Decoder[Option[Configuration]] =
        summon[Decoder[Configuration]].attempt.map(_.toOption)

      private given da: Decoder[Option[(Observation.Id, Configuration)]] = hc =>
        for
          id  <- hc.downField("id").as[Observation.Id]
          cfg <- hc.downField("configuration").as(using dc)
        yield cfg.tupleLeft(id)

      private given db: Decoder[(ConfigurationRequest, List[(Observation.Id, Configuration)])] = hc =>
        for
          req <- hc.as[ConfigurationRequest]
          obs <- hc.downFields("program", "observations", "matches").as(using Decoder.decodeList(using da))
        yield (req, obs.flatten)
 
      given Decoder[Response] = hc =>
        hc.downFields("configurationRequests", "matches")
          .as(using Decoder.decodeList(using db))

      def apply(rids: List[ConfigurationRequest.Id]) =
        s"""
          query {
            configurationRequests(
              WHERE: {
                id: {
                  IN: ${rids.asJson}
                }
              }
            ) {
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
                  target {
                    coordinates {
                      ra { 
                        hms 
                      }
                      dec { 
                        dms 
                      }
                    }
                    region {
                      rightAscensionArc {
                        type
                        start { hms }
                        end { hms }
                      }
                      declinationArc {
                        type
                        start { dms }
                        end { dms }
                      }
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
                program {
                  observations {
                    matches {
                      id
                      configuration {
                        conditions {
                          imageQuality
                          cloudExtinction
                          skyBackground
                          waterVapor
                        }
                        target {
                          coordinates {
                            ra { 
                              hms 
                            }
                            dec { 
                              dms 
                            }
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
          }
        """

    def selectAllRequestsForProgram2(pid: Program.Id) =
      s"""
        query {
          program(programId: "$pid") {
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
                  target {
                    coordinates {
                      ra { 
                        hms 
                      }
                      dec { 
                        dms 
                      }
                    }
                    region {
                      rightAscensionArc {
                        type
                        start { hms }
                        end { hms }
                      }
                      declinationArc {
                        type
                        start { dms }
                        end { dms }
                      }
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
      """

  }

  private object Statements {

    // select matching row, if any
    val SelectRequest: Query[(Observation.Id, Configuration), ConfigurationRequest] =
      sql"""
        SELECT
          c_configuration_request_id,
          c_status,
          c_justification,
          c_cloud_extinction,
          c_image_quality,
          c_sky_background,
          c_water_vapor,
          c_reference_ra,
          c_reference_dec,
          c_region_ra_arc_type,
          c_region_ra_arc_start,
          c_region_ra_arc_end,
          c_region_dec_arc_type,
          c_region_dec_arc_start,
          c_region_dec_arc_end,
          c_observing_mode_type,
          c_gmos_north_longslit_grating,
          c_gmos_south_longslit_grating
        FROM t_configuration_request
        WHERE (
          c_program_id = (select c_program_id from t_observation where c_observation_id = $observation_id) AND
          c_cloud_extinction = $cloud_extinction_preset AND
          c_image_quality = $image_quality_preset AND
          c_sky_background = $sky_background AND
          c_water_vapor = $water_vapor AND
          c_reference_ra is not distinct from ${right_ascension.opt} AND
          c_reference_dec is not distinct from ${declination.opt} AND
          c_region_ra_arc_type is not distinct from ${arc_type.opt} AND
          c_region_ra_arc_start is not distinct from ${right_ascension.opt} AND
          c_region_ra_arc_end is not distinct from ${right_ascension.opt} AND
          c_region_dec_arc_type is not distinct from ${arc_type.opt} AND
          c_region_dec_arc_start is not distinct from ${declination.opt} AND
          c_region_dec_arc_end is not distinct from ${declination.opt} AND



          c_observing_mode_type = $observing_mode_type AND
          c_gmos_north_longslit_grating is not distinct from ${gmos_north_grating.opt} AND
          c_gmos_south_longslit_grating is not distinct from ${gmos_south_grating.opt}
        ) 
      """.query(
        (
          configuration_request_id     *:
          configuration_request_status *:
          text_nonempty.opt            *:
          cloud_extinction_preset      *:
          image_quality_preset         *:
          sky_background               *:
          water_vapor                  *:
          right_ascension.opt          *:
          declination.opt              *:
          arc_type.opt                 *:
          right_ascension.opt          *:
          right_ascension.opt          *:
          arc_type.opt                 *:
          declination.opt              *:
          declination.opt              *:
          observing_mode_type          *:
          gmos_north_grating.opt       *:
          gmos_south_grating.opt
        ).emap:       
          { case 
            id                       *:
            status                   *:
            justification            *:
            cloudExtinction          *:
            imageQuality             *:
            skyBackground            *:
            waterVapor               *:
            rightAscension           *:
            declination              *:
            raArcType                *:
            raArcStart               *:
            raArcEnd                 *:
            decArcType               *:
            decArcStart              *:
            decArcEnd                *:
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

              val refCoords: Either[String, Option[Coordinates]] =
                (rightAscension, declination) match
                  case (Some(ra), Some(dec)) => Right(Some(Coordinates(ra, dec)))
                  case (None, None) => Right(None)
                  case _ => Left("Malformed reference coordinates.")

              def arc[A: Angular](tpe: Option[ArcType], start: Option[A], end: Option[A]): Either[String, Option[Arc[A]]] =
                (tpe, start, end) match
                  case (Some(ArcType.Empty), None, None) => Right(Some(Arc.Empty()))
                  case (Some(ArcType.Full), None, None) => Right(Some(Arc.Full()))
                  case (Some(ArcType.Partial), Some(a), Some(b)) => Right(Some(Arc.Partial(a, b)))
                  case (None, None, None) => Right(None)
                  case _ => Left(s"Malformed arc.")

              val raArc: Either[String, Option[Arc[RightAscension]]] =
                arc(raArcType, raArcStart, raArcEnd)
                
              val decArc: Either[String, Option[Arc[Declination]]] =
                arc(decArcType, decArcStart, decArcEnd)

              val region: Either[String, Option[Region]] =
                (raArc, decArc).tupled.flatMap:
                  case (Some(ra), Some(dec)) => Right(Some(Region(ra, dec)))
                  case (None, None) => Right(None)
                  case _ => Left("Malformed region.")

              val target: Either[String, Either[Coordinates, Region]] =
                (refCoords, region).tupled.flatMap:
                  case (Some(rc), None) => Right(Left(rc))
                  case (None, Some(r))  => Right(Right(r))
                  case _ => Left("Malformed configuration target.")

              mode.flatMap: m =>
                target.map: t =>
                  ConfigurationRequest(
                    id, 
                    status,
                    justification,
                    Configuration(
                      Conditions(
                        cloudExtinction,
                        imageQuality,
                        skyBackground,
                        waterVapor
                      ),
                      t,
                      m
                    )
                  )

          }
      ).contramap[(Observation.Id, Configuration)] { (oid, cfg) => 
        oid                                                       *:
        cfg.conditions.cloudExtinction                            *:
        cfg.conditions.imageQuality                               *:
        cfg.conditions.skyBackground                              *:
        cfg.conditions.waterVapor                                 *:
        cfg.target.left.toOption.map(_.ra)                        *:
        cfg.target.left.toOption.map(_.dec)                       *:
        cfg.target.toOption.map(_.raArc.tag)                      *:
        cfg.target.toOption.flatMap(Region.raArcStart.getOption)  *:
        cfg.target.toOption.flatMap(Region.raArcEnd.getOption)    *:
        cfg.target.toOption.map(_.decArc.tag)                     *:
        cfg.target.toOption.flatMap(Region.decArcStart.getOption) *:
        cfg.target.toOption.flatMap(Region.decArcEnd.getOption)   *:
        cfg.observingMode.tpe                                     *:
        cfg.observingMode.gmosNorthLongSlit.map(_.grating)        *:
        cfg.observingMode.gmosSouthLongSlit.map(_.grating)        *:
        EmptyTuple
      }

    // insert and return row, or return nothing if a matching row exists
    val InsertRequest: Query[(CreateConfigurationRequestInput, Configuration), ConfigurationRequest] =
      sql"""
        INSERT INTO t_configuration_request (
          c_program_id,
          c_justification,
          c_cloud_extinction,
          c_image_quality,
          c_sky_background,
          c_water_vapor,
          c_reference_ra,
          c_reference_dec,
          c_region_ra_arc_type,
          c_region_ra_arc_start,
          c_region_ra_arc_end,
          c_region_dec_arc_type,
          c_region_dec_arc_start,
          c_region_dec_arc_end,
          c_observing_mode_type,
          c_gmos_north_longslit_grating,
          c_gmos_south_longslit_grating
        ) VALUES (
          (select c_program_id from t_observation where c_observation_id = $observation_id),
          ${text_nonempty.opt},
          $cloud_extinction_preset,
          $image_quality_preset,
          $sky_background,
          $water_vapor,
          ${right_ascension.opt},
          ${declination.opt},
          ${arc_type.opt},
          ${right_ascension.opt},
          ${right_ascension.opt},
          ${arc_type.opt},
          ${declination.opt},
          ${declination.opt},
          $observing_mode_type,
          ${gmos_north_grating.opt},
          ${gmos_south_grating.opt}
        ) 
        ON CONFLICT DO NOTHING
        RETURNING
          c_configuration_request_id,
          c_status,
          c_justification,
          c_cloud_extinction,
          c_image_quality,
          c_sky_background,
          c_water_vapor,
          c_reference_ra,
          c_reference_dec,
          c_region_ra_arc_type,
          c_region_ra_arc_start,
          c_region_ra_arc_end,
          c_region_dec_arc_type,
          c_region_dec_arc_start,
          c_region_dec_arc_end,
          c_observing_mode_type,
          c_gmos_north_longslit_grating,
          c_gmos_south_longslit_grating
      """.query(
        (
          configuration_request_id     *:
          configuration_request_status *:
          text_nonempty.opt            *:
          cloud_extinction_preset      *:
          image_quality_preset         *:
          sky_background               *:
          water_vapor                  *:
          right_ascension.opt          *:
          declination.opt              *:
          arc_type.opt                 *:
          right_ascension.opt          *:
          right_ascension.opt          *:
          arc_type.opt                 *:
          declination.opt              *:
          declination.opt              *:
          observing_mode_type          *:
          gmos_north_grating.opt       *:
          gmos_south_grating.opt
        ).emap:       
          { case 
            id                       *:
            status                   *:
            justification            *:
            cloudExtinction          *:
            imageQuality             *:
            skyBackground            *:
            waterVapor               *:
            rightAscension           *:
            declination              *:
            raArcType                *:
            raArcStart               *:
            raArcEnd                 *:
            decArcType               *:
            decArcStart              *:
            decArcEnd                *:
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

              val refCoords: Either[String, Option[Coordinates]] =
                (rightAscension, declination) match
                  case (Some(ra), Some(dec)) => Right(Some(Coordinates(ra, dec)))
                  case (None, None) => Right(None)
                  case _ => Left("Malformed reference coordinates.")

              def arc[A: Angular](tpe: Option[ArcType], start: Option[A], end: Option[A]): Either[String, Option[Arc[A]]] =
                (tpe, start, end) match
                  case (Some(ArcType.Empty), None, None) => Right(Some(Arc.Empty()))
                  case (Some(ArcType.Full), None, None) => Right(Some(Arc.Full()))
                  case (Some(ArcType.Partial), Some(a), Some(b)) => Right(Some(Arc.Partial(a, b)))
                  case (None, None, None) => Right(None)
                  case _ => Left(s"Malformed arc.")

              val raArc: Either[String, Option[Arc[RightAscension]]] =
                arc(raArcType, raArcStart, raArcEnd)
                
              val decArc: Either[String, Option[Arc[Declination]]] =
                arc(decArcType, decArcStart, decArcEnd)

              val region: Either[String, Option[Region]] =
                (raArc, decArc).tupled.flatMap:
                  case (Some(ra), Some(dec)) => Right(Some(Region(ra, dec)))
                  case (None, None) => Right(None)
                  case _ => Left("Malformed region.")

              val target: Either[String, Either[Coordinates, Region]] =
                (refCoords, region).tupled.flatMap:
                  case (Some(rc), None) => Right(Left(rc))
                  case (None, Some(r))  => Right(Right(r))
                  case _ => Left("Malformed configuration target.")

              mode.flatMap: m =>
                target.map: t =>
                  ConfigurationRequest(
                    id, 
                    status,
                    justification,
                    Configuration(
                      Conditions(
                        cloudExtinction,
                        imageQuality,
                        skyBackground,
                        waterVapor
                      ),
                      t,
                      m
                    )
                  )

          }
      ).contramap[(CreateConfigurationRequestInput, Configuration)] { (input, cfg) => 
        input.oid                                                 *:
        input.SET.justification                                   *:
        cfg.conditions.cloudExtinction                            *:
        cfg.conditions.imageQuality                               *:
        cfg.conditions.skyBackground                              *:
        cfg.conditions.waterVapor                                 *:
        cfg.target.left.toOption.map(_.ra)                        *:
        cfg.target.left.toOption.map(_.dec)                       *:
        cfg.target.toOption.map(_.raArc.tag)                      *:
        cfg.target.toOption.flatMap(Region.raArcStart.getOption)  *:
        cfg.target.toOption.flatMap(Region.raArcEnd.getOption)    *:
        cfg.target.toOption.map(_.decArc.tag)                     *:
        cfg.target.toOption.flatMap(Region.decArcStart.getOption) *:
        cfg.target.toOption.flatMap(Region.decArcEnd.getOption)   *:
        cfg.observingMode.tpe                                     *:
        cfg.observingMode.gmosNorthLongSlit.map(_.grating)        *:
        cfg.observingMode.gmosSouthLongSlit.map(_.grating)        *:
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
    def updateRequests(SET: ConfigurationRequestPropertiesInput.Update, which: AppliedFragment): AppliedFragment =
      val statusExpr: AppliedFragment = SET.status.fold(void"c_status")(sql"$configuration_request_status".apply)
      val justExpr = SET.justification.fold(void"null", void"c_justification", sql"$text_nonempty".apply)
      void"""
        update t_configuration_request
        set c_status = """ |+| statusExpr |+| void", c_justification = " |+| justExpr |+|
      void" where c_configuration_request_id in (" |+| which |+| void""")
        returning c_configuration_request_id
      """

    val SelectActiveNonCalibrations: Query[Program.Id, Observation.Id] =
      sql"""
        select c_observation_id
        from t_observation
        where c_program_id = $program_id
        and c_workflow_user_state is distinct from 'inactive'::e_workflow_user_state
        and c_calibration_role is null
      """.query(observation_id)
    
  }

}

