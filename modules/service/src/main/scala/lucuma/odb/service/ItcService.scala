// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.data.OptionT
import cats.effect.Async
import cats.effect.Concurrent
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.syntax.spawn.*
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.Stream
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.itc.client.IntegrationTimeResult
import lucuma.itc.client.ItcClient
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.odb.data.Md5Hash
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.util.Codecs.*
import org.typelevel.log4cats.Logger
import skunk.*
import skunk.codec.text.text
import skunk.implicits.*

import java.security.MessageDigest
import scala.concurrent.duration.*

sealed trait ItcService[F[_]] {

  import ItcService.AsterismResult
  import ItcService.Error

  /**
   * Obtains the ITC results for a single target, first checking the cache and
   * then performing a query to the remote ITC service if necessary.
   */
  def lookup(
    programId:     Program.Id,
    observationId: Observation.Id
  )(using NoTransaction[F]): F[Either[Error, Option[AsterismResult]]]

  /**
   * Using the provided generator parameters, calls the remote ITC service to
   * obtain the AsterismResult, if possible, then caches it for future reference.
   */
  def callRemote(
    programId:     Program.Id,
    observationId: Observation.Id,
    params:        GeneratorParams
  )(using NoTransaction[F]): F[Either[Error, AsterismResult]]

  /**
   * Selects the cached ITC results for a single observation, if available and
   * still valid.  Does not perform a remote ITC service call if not available.
   */
  def selectOne(
    programId:    Program.Id,
    observatinId: Observation.Id,
    params:       GeneratorParams
  )(using Transaction[F]): F[Option[AsterismResult]]

  /**
   * Selects the cached ITC results for a program, for those observations where
   * it is available and still valid.  Does not perform a remote ITC service
   * call if not available.
   */
  def selectAll(
    programId: Program.Id,
    params:    Map[Observation.Id, GeneratorParams]
  )(using Transaction[F]): F[Map[Observation.Id, AsterismResult]]

}

object ItcService {

  sealed trait Error {
    def format: String
  }

  object Error {

    case class ObservationDefinitionError(
      errors: NonEmptyList[GeneratorParamsService.Error]
    ) extends Error {
      def format: String = {
        val conflict = Option.when(errors.exists(_ === GeneratorParamsService.Error.ConflictingData))(
          "Observation would require multiple instrument configurations to observe all the targets in the asterism."
        )

        val missing = errors.collect {
          case GeneratorParamsService.Error.MissingData(tid, paramName) =>
            s"${tid.fold("") { tid => s"(target $tid) " }}$paramName"
        } match {
          case Nil    => none[String]
          case params => s"ITC cannot be queried until the following parameters are defined: ${params.intercalate(", ")}".some
        }

        (conflict.toList ++ missing.toList).intercalate("\n")
      }
    }

    case class RemoteServiceErrors(
      problems: NonEmptyList[(Target.Id, String)]
    ) extends Error {
      def format: String = {
        val ps = problems.map { case (tid, msg) => s"Target '$tid': $msg"}
        s"ITC returned errors: ${ps.intercalate(", ")}"
      }
    }
  }

  case class TargetResult(
    targetId: Target.Id,
    input:    SpectroscopyIntegrationTimeInput,
    value:    IntegrationTime
  ) {
    def totalTime: Option[TimeSpan] = {
      val total = BigInt(value.exposureTime.toMicroseconds) * value.exposures.value
      Option.when(total.isValidLong)(TimeSpan.fromMicroseconds(total.longValue)).flatten
    }
  }

  object TargetResult {

    given Order[TargetResult] =
      Order.by { s => (s.totalTime, s.targetId) }

    import lucuma.odb.json.time.query.given

    given Encoder[TargetResult] =
      Encoder.instance { s =>
        Json.obj(
          "targetId"      -> s.targetId.asJson,
          "exposureTime"  -> s.value.exposureTime.asJson,
          "exposures"     -> s.value.exposures.value.asJson,
          "signalToNoise" -> s.value.signalToNoise.asJson
        )
      }
  }

  case class AsterismResult(
    value: Zipper[TargetResult]
  )

  object AsterismResult {

    given Encoder[AsterismResult] =
      Encoder.instance { rs =>
        Json.obj(
          "result" -> rs.value.focus.asJson,
          "all"    -> rs.value.toList.asJson
        )
      }

  }

  private def hash(input: SpectroscopyIntegrationTimeInput): Md5Hash =
    Md5Hash.unsafeFromByteArray {
      MessageDigest
        .getInstance("MD5")
        .digest(input.asJson.noSpaces.getBytes("UTF-8"))
    }

  def pollVersionsForever[F[_]: Async: Temporal: Logger](
    client:     Resource[F, ItcClient[F]],
    session:    Resource[F, Session[F]],
    pollPeriod: FiniteDuration
  ): F[Unit] = {
    val pollOnce: F[Unit] =
      for {
        v <- client.use(_.versions)
        _ <- session.use { s => s.transaction.use(_ => s.execute(Statements.UpdateItcVersion)(v.server.some, v.data)) }
      } yield ()

    val pollOnceLogError: F[Unit] =
      pollOnce.handleErrorWith { t =>
        Logger[F].info(t)(s"Error while polling ITC versions: ${t.getMessage}")
      }

    Stream
      .fixedDelay(pollPeriod)
      .zip(Stream.repeatEval(pollOnceLogError))
      .compile
      .drain
      .start
      .void

  }

  def instantiate[F[_]: Concurrent](client: ItcClient[F])(using Services[F]): ItcService[F] =
    new ItcService[F] {

      import Error.*

      override def lookup(
        pid: Program.Id,
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[Error, Option[AsterismResult]]] =
        (for {
          pr     <- EitherT(attemptLookup(pid, oid))
          result <- pr.traverse { case (params, oa) => oa.fold(EitherT(callRemote(pid, oid, params)))(r => EitherT.pure(r)) }
        } yield result).value

      override def callRemote(
        pid:    Program.Id,
        oid:    Observation.Id,
        params: GeneratorParams
      )(using NoTransaction[F]): F[Either[Error, AsterismResult]] =
        (for {
          r <- EitherT(callRemote(params))
          _ <- EitherT.liftF(services.transactionally(insertOrUpdate(pid, oid, r)))
        } yield r).value

      // Selects the parameters then selects the previously stored result set, if any.
      private def attemptLookup(
        pid: Program.Id,
        oid: Observation.Id
      )(using NoTransaction[F]): F[Either[Error, Option[(GeneratorParams, Option[AsterismResult])]]] =
        services.transactionally {
          (for {
            p <- EitherT(generatorParamsService.selectOne(pid, oid).map(_.leftMap(ObservationDefinitionError(_))))
            r <- EitherT.liftF(p.fold(none[AsterismResult].pure[F])(selectOne(pid, oid, _)))
          } yield p.tupleRight(r)).value
        }

      private val itcInputs: GeneratorParams => NonEmptyList[(Target.Id, SpectroscopyIntegrationTimeInput)] = {
        case GeneratorParams.GmosNorthLongSlit(specs, _) => specs
        case GeneratorParams.GmosSouthLongSlit(specs, _) => specs
      }

      // Selects the asterism result as a whole by selecting all the individual
      // target results.  If any individual result is not found then the asterism
      // as a whole is considered not found.
      override def selectOne(
        pid:    Program.Id,
        oid:    Observation.Id,
        params: GeneratorParams
      )(using Transaction[F]): F[Option[AsterismResult]] = {

        def selectSingleTarget(
          tid:   Target.Id,
          input: SpectroscopyIntegrationTimeInput
        ): F[Option[TargetResult]] =
          session
            .option(Statements.SelectOneItcResult)(pid, oid, tid)
            .map(_.collect { case (h, time) if h === hash(input) =>
              TargetResult(tid, input, time)
            })

        itcInputs(params)
          .traverse { case (tid, input) => OptionT(selectSingleTarget(tid, input)) }
          .value
          .map(_.map(lst => AsterismResult(Zipper.fromNel(lst).focusMax)))
      }

      override def selectAll(
        pid:    Program.Id,
        params: Map[Observation.Id, GeneratorParams]
      )(using Transaction[F]): F[Map[Observation.Id, AsterismResult]] =
        session
          .execute(Statements.SelectAllItcResults)(pid)
          .map(
            _.groupBy(_._1)
             .map { case (oid, lst) =>

               // Get the cached result, if any, for each target in the
               // observation's asterism.
               val cachedResults: Map[Target.Id, (Md5Hash, IntegrationTime)] =
                 lst.map { case (_, tid, h, time) => tid -> (h, time) }.toMap

               // Get the GeneratorParams for the observation, lookup the ITC
               // inputs (there's one per target), then find the corresponding
               // cached result.  Assuming the hash matches, it is still valid.
               // If they are all still valid, we can create an AsterismResult.
               val asterismResult: Option[AsterismResult] =
                 params
                   .get(oid)
                   .map(itcInputs)
                   .flatMap(
                     _.traverse { case (tid, input) =>
                       cachedResults.get(tid).collect { case (h, time) if hash(input) === h =>
                         TargetResult(tid, input, time)
                       }
                     }
                   )
                   .map { lst => // NonEmptyList[TargetResult]
                     AsterismResult(Zipper.fromNel(lst).focusMax)
                   }

               (oid, asterismResult)
             }
             .collect { case (oid, Some(a)) => (oid, a) }
          )

      private def callRemote(
        params:   GeneratorParams
      )(using NoTransaction[F]): F[Either[Error, AsterismResult]] =
        params match {
          case GeneratorParams.GmosNorthLongSlit(itc, _) => callRemoteSpectroscopy(itc)
          case GeneratorParams.GmosSouthLongSlit(itc, _) => callRemoteSpectroscopy(itc)
        }

      private def callRemoteSpectroscopy(
        targets:  NonEmptyList[(Target.Id, SpectroscopyIntegrationTimeInput)]
      )(using NoTransaction[F]): F[Either[Error, AsterismResult]] =
        targets.traverse { case (tid, si) =>
          client.spectroscopy(si, useCache = false).map {
            case IntegrationTimeResult(_, results) =>
              TargetResult(tid, si, results.head).rightNel
          }
          .handleError { t => (tid, t.getMessage).leftNel }
        }.map(_.sequence.bimap(
          errors  => RemoteServiceErrors(errors),
          targets => AsterismResult(Zipper.fromNel(targets).focusMax)
        ))

      private def insertOrUpdate(
        pid:       Program.Id,
        oid:       Observation.Id,
        resultSet: AsterismResult
      )(using Transaction[F]): F[Unit] = {

        def insertOrUpdateSingleTarget(success: TargetResult): F[Unit] = {
          val h = hash(success.input)
          session.execute(Statements.InsertOrUpdateItcResult)(
            pid,
            oid,
            success.targetId,
            h,
            success.value.exposureTime,
            success.value.exposures,
            success.value.signalToNoise,
            h,
            success.value.exposureTime,
            success.value.exposures,
            success.value.signalToNoise,
          ).void
        }

        resultSet.value.traverse(insertOrUpdateSingleTarget).void
      }

    }

  object Statements {
    private val integration_time: Codec[IntegrationTime] =
      (time_span *: pos_int *: signal_to_noise).to[IntegrationTime]

    val UpdateItcVersion: Command[(
      Option[String],
      Option[String]
    )] =
      sql"""
        UPDATE t_itc_version
           SET c_version = ${text.opt},
               c_data    = ${text.opt}
      """.command

    val SelectOneItcResult: Query[(
      Program.Id,
      Observation.Id,
      Target.Id
    ), (Md5Hash, IntegrationTime)] =
      sql"""
        SELECT
          c_hash,
          c_exposure_time,
          c_exposure_count,
          c_signal_to_noise
        FROM t_itc_result
        WHERE c_program_id     = $program_id     AND
              c_observation_id = $observation_id AND
              c_target_id      = $target_id
      """.query(md5_hash *: integration_time)

    val SelectAllItcResults: Query[Program.Id, (Observation.Id, Target.Id, Md5Hash, IntegrationTime)] =
      sql"""
        SELECT
          c_observation_id,
          c_target_id,
          c_hash,
          c_exposure_time,
          c_exposure_count,
          c_signal_to_noise
        FROM t_itc_result
        WHERE c_program_id = $program_id
      """.query(observation_id *: target_id *: md5_hash *: integration_time)

    val InsertOrUpdateItcResult: Command[(
      Program.Id,
      Observation.Id,
      Target.Id,
      Md5Hash,
      TimeSpan,
      PosInt,
      SignalToNoise,
      Md5Hash,
      TimeSpan,
      PosInt,
      SignalToNoise
    )] =
      sql"""
        INSERT INTO t_itc_result (
          c_program_id,
          c_observation_id,
          c_target_id,
          c_hash,
          c_exposure_time,
          c_exposure_count,
          c_signal_to_noise
        ) SELECT
          $program_id,
          $observation_id,
          $target_id,
          $md5_hash,
          $time_span,
          $pos_int,
          $signal_to_noise
        ON CONFLICT ON CONSTRAINT t_itc_result_pkey DO UPDATE
          SET c_hash            = $md5_hash,
              c_exposure_time   = $time_span,
              c_exposure_count  = $pos_int,
              c_signal_to_noise = $signal_to_noise
      """.command


  }

}