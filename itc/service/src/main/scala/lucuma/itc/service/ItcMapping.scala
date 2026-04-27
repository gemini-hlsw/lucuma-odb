// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import buildinfo.BuildInfo
import cats.*
import cats.data.NonEmptyChain
import cats.effect.*
import cats.syntax.all.*
import eu.timepit.refined.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.*
import grackle.circe.CirceMapping
import io.circe.syntax.*
import lucuma.core.math.SignalToNoise
import lucuma.itc.*
import lucuma.itc.cache.BinaryEffectfulCache
import lucuma.itc.input.*
import lucuma.itc.input.customSed.CustomSed
import lucuma.itc.service.config.*
import lucuma.itc.service.encoders.given
import lucuma.itc.service.requests.*
import lucuma.itc.service.syntax.all.*
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.trace.Tracer

import scala.io.Source
import scala.util.Using

import QueryCompiler.*

object ItcMapping extends ItcCacheOrRemote with Version {

  // In principle this is a pure operation because resources are constant values, but the potential
  // for error in dev is high and it's nice to handle failures in `F`.
  def loadSchema[F[_]: Sync]: F[Schema] =
    Sync[F]
      .blocking:
        Using(Source.fromResource("graphql/itc.graphql", getClass().getClassLoader())): src =>
          Schema(src.mkString).toEither
            .fold(
              x => sys.error(s"Invalid schema: ${x.toList.mkString(", ")}"),
              identity
            )
      .flatMap(_.liftTo[F])

  def versions[F[_]: Applicative](
    environment: ExecutionEnvironment
  ): F[Result[ItcVersions]] =
    Result(ItcVersions(version(environment).value, BuildInfo.ocslibHash.some)).pure[F]

  private val buildError: Throwable => Error =
    case SourceTooBright(hw)      => Error.SourceTooBright(hw)
    case WavelengthOutOfRange(wv) => Error.WavelengthAtOutOfRange(wv)
    case t                        => Error.General(s"Error calculating ITC: ${t.getMessage}")

  private def errorToProblem(error: Error, targetIndex: Int): Problem =
    Problem(error.message, extensions = ErrorExtension(targetIndex, error).asJsonObject.some)

  extension (timeResult: CalculationResult)
    private def toResult: Result[CalculationResult] =
      timeResult.targetTimes.collectErrors.fold(Result.success(timeResult)): errors =>
        Result.Warning(errors.map(errorToProblem), timeResult)

  extension (timeAndGraphsResult: SpectroscopyTimeAndGraphsResult)
    private def toResult: Result[SpectroscopyTimeAndGraphsResult] =
      val optErrors: Option[NonEmptyChain[(Error, Int)]] =
        timeAndGraphsResult.targetOutcomes.collectErrors
      optErrors.fold(Result.success(timeAndGraphsResult))(errors =>
        Result.Warning(errors.map(errorToProblem), timeAndGraphsResult)
      )

  extension [F[_]: MonadThrow, A](f: F[Result[A]])
    private def toGraphQLErrors: F[Result[A]] =
      f.handleError: t =>
        Result.failure(Problem(t.getMessage))

  def calculateSpectroscopyIntegrationTime[F[
    _
  ]: MonadThrow: Parallel: Logger: CustomSed.Resolver: Tracer](
    environment:     ExecutionEnvironment,
    cache:           BinaryEffectfulCache[F],
    itc:             Itc[F],
    config:          Config
  )(
    asterismRequest: AsterismSpectroscopyTimeRequest
  ): F[Result[CalculationResult]] =
    Tracer[F]
      .span("process target_requests_parallel")
      .surround:
        asterismRequest.toTargetRequests
          .parTraverse: (targetRequest: TargetSpectroscopyTimeRequest) =>
            spectroscopyFromCacheOrRemote(targetRequest)(itc, cache, config).attempt
              .map: (result: Either[Throwable, TargetIntegrationTime]) =>
                TargetIntegrationTimeOutcome:
                  result.leftMap(buildError)
      .flatMap: (targetOutcomes: NonEmptyChain[TargetIntegrationTimeOutcome]) =>
        Tracer[F]
          .span("build spectroscopy_result")
          .surround:
            CalculationResult(
              ItcVersions(version(environment).value, BuildInfo.ocslibHash.some),
              AsterismIntegrationTimeOutcomes(targetOutcomes)
            ).toResult.pure[F]
      .onError: t =>
        Logger[F]
          .error(t):
            s"Error calculating spectroscopy integration time for input: $asterismRequest"

  def calculateImagingIntegrationTime[F[
    _
  ]: MonadThrow: Parallel: Logger: CustomSed.Resolver: Tracer](
    environment: ExecutionEnvironment,
    cache:       BinaryEffectfulCache[F],
    itc:         Itc[F],
    config:      Config
  )(asterismRequest: AsterismImagingTimeRequest): F[Result[CalculationResult]] =
    Tracer[F]
      .span("process imaging_targets_parallel")
      .surround:
        asterismRequest.toTargetRequests
          .parTraverse: (targetRequest: TargetImagingTimeRequest) =>
            imagingFromCacheOrRemote(targetRequest)(itc, cache, config).attempt
              .map: (result: Either[Throwable, TargetIntegrationTime]) =>
                TargetIntegrationTimeOutcome:
                  result
                    .leftMap(buildError)
                    .map: (integrationTime: TargetIntegrationTime) =>
                      asterismRequest.imagingMode match
                        case ObservingMode.ImagingMode.GmosNorth(_, _, _) |
                            ObservingMode.ImagingMode.GmosSouth(_, _, _) =>
                          integrationTime
                            .focusIndex(1) // For gmos focus on the second CCD
                            .getOrElse(integrationTime)
                        case ObservingMode.ImagingMode.Flamingos2(_, _, _) =>
                          integrationTime
      .flatMap: (targetOutcomes: NonEmptyChain[TargetIntegrationTimeOutcome]) =>
        Tracer[F]
          .span("build imaging_result")
          .surround:
            CalculationResult(
              ItcVersions(version(environment).value, BuildInfo.ocslibHash.some),
              AsterismIntegrationTimeOutcomes(targetOutcomes)
            ).toResult.pure[F]
      .onError: t =>
        Logger[F]
          .error(t)(s"Error calculating imaging integration time for input: $asterismRequest")
      .toGraphQLErrors

  extension (tAndG: TargetTimeAndGraphs)
    private def roundSigFigs(significantFigures: Option[SignificantFigures]): TargetTimeAndGraphs =
      significantFigures.fold(tAndG): sigfigs =>
        tAndG.copy(graphs = roundTargetGraphsSigFigs(Some(sigfigs))(tAndG.graphs))

  private def roundTargetGraphsSigFigs(
    significantFigures: Option[SignificantFigures]
  )(targetGraphs: TargetGraphs): TargetGraphs =
    significantFigures.fold(targetGraphs): sigfigs =>
      val graphs            = targetGraphs.graphData.map(_.adjustSignificantFigures(sigfigs))
      val ccds              = targetGraphs.ccds.map(_.adjustSignificantFigures(sigfigs))
      val peakFinalSNRatio  = targetGraphs.peakFinalSNRatio.adjustSignificantFigures(sigfigs)
      val peakSingleSNRatio = targetGraphs.peakSingleSNRatio.adjustSignificantFigures(sigfigs)
      val atWvFinalSNRatio  =
        targetGraphs.atWavelengthFinalSNRatio.map(_.adjustSignificantFigures(sigfigs))
      val atWvSingleSNRatio =
        targetGraphs.atWavelengthSingleSNRatio.map(_.adjustSignificantFigures(sigfigs))
      TargetGraphs(
        ccds,
        graphs,
        peakFinalSNRatio,
        atWvFinalSNRatio,
        peakSingleSNRatio,
        atWvSingleSNRatio
      )

  def spectroscopyIntegrationTimeAndGraphs[F[
    _
  ]: MonadThrow: Parallel: Logger: CustomSed.Resolver: Tracer](
    environment:     ExecutionEnvironment,
    cache:           BinaryEffectfulCache[F],
    itc:             Itc[F],
    config:          Config
  )(
    asterismRequest: AsterismSpectroscopyTimeRequest,
    figures:         Option[SignificantFigures]
  ): F[Result[SpectroscopyTimeAndGraphsResult]] =
    Tracer[F]
      .span("spectroscopy time_and_graphs_combined")
      .surround:
        asterismRequest.toTargetRequests
          .parTraverse: (targetRequest: TargetSpectroscopyTimeRequest) =>
            timeAndGraphsFromCacheOrRemote(targetRequest)(itc, cache, config).attempt
              .map: (result: Either[Throwable, TargetTimeAndGraphs]) =>
                TargetTimeAndGraphsOutcome:
                  result.bimap(buildError, _.roundSigFigs(figures))
      .flatMap: (targetTimesAndGraphs: NonEmptyChain[TargetTimeAndGraphsOutcome]) =>
        Tracer[F]
          .span("build time_and_graphs_result")
          .surround:
            SpectroscopyTimeAndGraphsResult(
              ItcVersions(version(environment).value, BuildInfo.ocslibHash.some),
              AsterismTimeAndGraphsOutcomes(targetTimesAndGraphs)
            ).toResult.pure[F]
      .onError: t =>
        Logger[F]
          .error(t):
            s"Error calculating spectroscopy integration time and graph for input: $asterismRequest"

  def apply[F[_]: Sync: Logger: Parallel: Tracer: CustomSed.Resolver](
    environment: ExecutionEnvironment,
    cache:       BinaryEffectfulCache[F],
    itc:         Itc[F],
    config:      Config
  ): F[Mapping[F]] =
    loadSchema[F].map { loadedSchema =>
      new CirceMapping[F] {

        override def parserConfig: GraphQLParser.Config = // set a more reasonable input depth limit
          GraphQLParser.defaultConfig.copy(maxInputValueDepth = 16)

        val schema: Schema    = loadedSchema
        val QueryType         = schema.ref("Query")
        val BigDecimalType    = schema.ref("BigDecimal")
        val LongType          = schema.ref("Long")
        val NonNegIntType     = schema.ref("NonNegInt")
        val SignalToNoiseType = schema.ref("SignalToNoise")

        val typeMappings: TypeMappings =
          List(
            ObjectMapping(
              tpe = QueryType,
              fieldMappings = List(
                RootEffect.computeEncodable("versions")((_, _) => versions(environment)),
                RootEffect.computeEncodable("spectroscopy") { (_, env) =>
                  env
                    .getR[SpectroscopyInput]("input")
                    .flatMap(AsterismSpectroscopyTimeRequest.fromInput)
                    .flatTraverse:
                      calculateSpectroscopyIntegrationTime[F](environment, cache, itc, config)
                    .toGraphQLErrors
                },
                RootEffect.computeEncodable("imaging") { (_, env) =>
                  env
                    .getR[ImagingInput]("input")
                    .flatMap(AsterismImagingTimeRequest.fromInput)
                    .flatTraverse:
                      calculateImagingIntegrationTime(environment, cache, itc, config)
                    .toGraphQLErrors
                },
                RootEffect.computeEncodable("spectroscopyIntegrationTimeAndGraphs") { (_, env) =>
                  env
                    .getR[SpectroscopyIntegrationTimeAndGraphsInput]("input")
                    .flatMap: input =>
                      AsterismSpectroscopyTimeRequest
                        .fromInput(input)
                        .map((_, input.significantFigures))
                    .flatTraverse: (tr, fig) =>
                      spectroscopyIntegrationTimeAndGraphs(environment, cache, itc, config)(tr, fig)
                    .toGraphQLErrors
                }
              )
            ),
            LeafMapping[BigDecimal](BigDecimalType),
            LeafMapping[Long](LongType),
            LeafMapping[NonNegInt](NonNegIntType),
            LeafMapping[SignalToNoise](SignalToNoiseType)
          )

        override val selectElaborator: SelectElaborator =
          def handle[A](input: Result[A]): Elab[Unit] =
            Elab.liftR(input).flatMap(i => Elab.env("input" -> i))

          SelectElaborator {
            case (QueryType, "spectroscopy", List(SpectroscopyInput.Binding("input", input))) =>
              handle(input)
            case (QueryType, "imaging", List(ImagingInput.Binding("input", input)))           =>
              handle(input)
            case (QueryType,
                  "spectroscopyIntegrationTimeAndGraphs",
                  List(SpectroscopyIntegrationTimeAndGraphsInput.Binding("input", input))
                ) =>
              handle(input)
          }
      }
    }
}
