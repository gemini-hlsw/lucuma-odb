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
import eu.timepit.refined.types.numeric.PosInt
import grackle.*
import grackle.circe.CirceMapping
import io.circe.syntax.*
import lucuma.core.enums.ExecutionEnvironment as _
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.core.util.TimeSpan
import lucuma.itc.*
import lucuma.itc.cache.BinaryEffectfulCache
import lucuma.itc.input.*
import lucuma.itc.input.customSed.CustomSed
import lucuma.itc.service.config.*
import lucuma.itc.service.encoders.given
import lucuma.itc.service.requests.*
import lucuma.itc.service.syntax.all.*
import org.typelevel.log4cats.Logger

import scala.io.Source
import scala.util.Using

import QueryCompiler.*

object ItcMapping extends ItcCacheOrRemote with Version {

  // In principle this is a pure operation because resources are constant values, but the potential
  // for error in dev is high and it's nice to handle failures in `F`.
  def loadSchema[F[_]: Sync]: F[Schema] =
    Sync[F]
      .defer:
        Using(Source.fromResource("graphql/itc.graphql", getClass().getClassLoader())): src =>
          Schema(src.mkString).toEither.fold(
            x => sys.error(s"Invalid schema: ${x.toList.mkString(", ")}"),
            identity
          )
        .liftTo[F]

  def versions[F[_]: Applicative](
    environment: ExecutionEnvironment
  ): F[Result[ItcVersions]] =
    Result(ItcVersions(version(environment).value, BuildInfo.ocslibHash.some)).pure[F]

  private val buildError: Throwable => Error =
    case SourceTooBright(hw) => Error.SourceTooBright(hw)
    case t                   => Error.General(s"Error calculating ITC: ${t.getMessage}")

  private def errorToProblem(error: Error, targetIndex: Int): Problem =
    Problem(error.message, extensions = ErrorExtension(targetIndex, error).asJsonObject.some)

  extension (timeResult: CalculationResult)
    private def toResult: Result[CalculationResult] =
      timeResult.targetTimes.collectErrors.fold(Result.success(timeResult)): errors =>
        Result.Warning(errors.map(errorToProblem), timeResult)

  extension (graphResult: SpectroscopyGraphsResult)
    private def toResult: Result[SpectroscopyGraphsResult] =
      graphResult.targetGraphs.collectErrors.fold(Result.success(graphResult)): errors =>
        Result.Warning(errors.map(errorToProblem), graphResult)

  extension [F[_]: MonadThrow, A](f: F[Result[A]])
    private def toGraphQLErrors: F[Result[A]] =
      f.handleError: t =>
        Result.failure(Problem(t.getMessage))

  def calculateSpectroscopyIntegrationTime[F[
    _
  ]: MonadThrow: Parallel: Logger: CustomSed.Resolver](
    environment:     ExecutionEnvironment,
    cache:           BinaryEffectfulCache[F],
    itc:             Itc[F],
    config:          Config
  )(
    asterismRequest: AsterismSpectroscopyTimeRequest
  ): F[Result[CalculationResult]] =
    asterismRequest.toTargetRequests
      .parTraverse: (targetRequest: TargetSpectroscopyTimeRequest) =>
        spectroscopyFromCacheOrRemote(targetRequest)(itc, cache, config).attempt
          .map: (result: Either[Throwable, TargetIntegrationTime]) =>
            TargetIntegrationTimeOutcome:
              result.leftMap(buildError)
      .map: (targetOutcomes: NonEmptyChain[TargetIntegrationTimeOutcome]) =>
        CalculationResult(
          ItcVersions(version(environment).value, BuildInfo.ocslibHash.some),
          asterismRequest.specMode,
          AsterismIntegrationTimeOutcomes(targetOutcomes),
          asterismRequest.exposureTimeMode
        ).toResult
      .onError: t =>
        Logger[F]
          .error(t):
            s"Error calculating spectroscopy integration time for input: $asterismRequest"

  def calculateImagingIntegrationTime[F[
    _
  ]: MonadThrow: Parallel: Logger: CustomSed.Resolver](
    environment: ExecutionEnvironment,
    cache:       BinaryEffectfulCache[F],
    itc:         Itc[F],
    config:      Config
  )(asterismRequest: AsterismImagingTimeRequest): F[Result[CalculationResult]] =
    asterismRequest.toTargetRequests
      .parTraverse: (targetRequest: TargetImagingTimeRequest) =>
        imagingFromCacheOrRemote(targetRequest)(itc, cache, config).attempt
          .map: (result: Either[Throwable, TargetIntegrationTime]) =>
            TargetIntegrationTimeOutcome:
              result
                .leftMap(buildError)
                .map: (integrationTime: TargetIntegrationTime) =>
                  asterismRequest.imagingMode match
                    case ObservingMode.ImagingMode.GmosNorth(_, _) |
                        ObservingMode.ImagingMode.GmosSouth(_, _) =>
                      integrationTime
                        .focusIndex(1) // For gmos focus on the second CCD
                        .getOrElse(integrationTime)
                    case ObservingMode.ImagingMode.Flamingos2(_) =>
                      integrationTime
      .map: (targetOutcomes: NonEmptyChain[TargetIntegrationTimeOutcome]) =>
        CalculationResult(
          ItcVersions(version(environment).value, BuildInfo.ocslibHash.some),
          asterismRequest.imagingMode,
          AsterismIntegrationTimeOutcomes(targetOutcomes),
          asterismRequest.exposureTimeMode
        ).toResult
      .onError: t =>
        Logger[F]
          .error(t)(s"Error calculating imaging integration time for input: $asterismRequest")
      .toGraphQLErrors

  private def buildTargetGraphsResult(significantFigures: Option[SignificantFigures])(
    graphResult: TargetGraphsCalcResult
  ): TargetGraphsResult = {
    val graphs: NonEmptyChain[ItcGraphGroup] =
      significantFigures.fold(graphResult.data): v =>
        graphResult.data.map(_.adjustSignificantFigures(v))

    val ccds: NonEmptyChain[ItcCcd] =
      significantFigures.fold(graphResult.ccds): v =>
        graphResult.ccds.map(_.adjustSignificantFigures(v))

    val peakFinalSNRatio =
      significantFigures.fold(graphResult.peakFinalSNRatio):
        graphResult.peakFinalSNRatio.adjustSignificantFigures

    val peakSingleSNRatio =
      significantFigures.fold(graphResult.peakSingleSNRatio):
        graphResult.peakSingleSNRatio.adjustSignificantFigures

    val atWvFinalSNRatio: Option[TotalSN] =
      significantFigures.fold(graphResult.atWavelengthFinalSNRatio): s =>
        graphResult.atWavelengthFinalSNRatio.map(_.adjustSignificantFigures(s))

    val atWvSingleSNRatio: Option[SingleSN] =
      significantFigures.fold(graphResult.atWavelengthSingleSNRatio): s =>
        graphResult.atWavelengthSingleSNRatio.map(_.adjustSignificantFigures(s))

    TargetGraphsResult(
      TargetGraphs(
        ccds,
        graphs.flatMap(_.graphs),
        peakFinalSNRatio,
        atWvFinalSNRatio,
        peakSingleSNRatio,
        atWvSingleSNRatio
      ),
      graphResult.bandOrLine
    )
  }

  def spectroscopyGraphs[F[_]: MonadThrow: Parallel: Logger: CustomSed.Resolver](
    environment: ExecutionEnvironment,
    cache:       BinaryEffectfulCache[F],
    itc:         Itc[F],
    config:      Config
  )(asterismRequest: AsterismGraphRequest): F[Result[SpectroscopyGraphsResult]] =
    asterismRequest.toTargetRequests
      .parTraverse: (targetRequest: TargetGraphRequest) =>
        graphsFromCacheOrRemote(targetRequest)(itc, cache, config).attempt
          .map: (result: Either[Throwable, TargetGraphsCalcResult]) =>
            TargetGraphsOutcome:
              result.bimap(buildError, buildTargetGraphsResult(asterismRequest.significantFigures))
      .map: (targetGraphs: NonEmptyChain[TargetGraphsOutcome]) =>
        SpectroscopyGraphsResult(
          ItcVersions(version(environment).value, BuildInfo.ocslibHash.some),
          AsterismTargetGraphsOutcomes(targetGraphs)
        ).toResult
      .onError: t =>
        Logger[F]
          .error(t)(s"Error calculating spectroscopy graphs for input: $asterismRequest")
      .toGraphQLErrors

  private def buildAsterismGraphRequest(
    asterismRequest: AsterismSpectroscopyTimeRequest,
    figures:         Option[SignificantFigures]
  )(integrationTimes: AsterismIntegrationTimes): AsterismGraphRequest =
    val brightestTarget: TargetIntegrationTime = integrationTimes.value.focus
    val selectedVariation: IntegrationTime     = brightestTarget.times.focus
    val expTime: TimeSpan                      = selectedVariation.exposureTime
    val expCount: PosInt                       = selectedVariation.exposureCount

    AsterismGraphRequest(
      asterismRequest.asterism,
      GraphParameters(
        asterismRequest.exposureTimeMode.at,
        asterismRequest.specMode,
        asterismRequest.constraints,
        expTime,
        expCount
      ),
      figures
    )

  def spectroscopyIntegrationTimeAndGraphs[F[
    _
  ]: MonadThrow: Parallel: Logger: CustomSed.Resolver](
    environment:     ExecutionEnvironment,
    cache:           BinaryEffectfulCache[F],
    itc:             Itc[F],
    config:          Config
  )(
    asterismRequest: AsterismSpectroscopyTimeRequest,
    figures:         Option[SignificantFigures]
  ): F[Result[SpectroscopyTimeAndGraphsResult]] =
    ResultT(calculateSpectroscopyIntegrationTime(environment, cache, itc, config)(asterismRequest))
      .flatMap: (specTimeResults: CalculationResult) =>
        specTimeResults.targetTimes.partitionErrors
          .bimap(
            // If there was an error computing integration times, we cannot compute the graphs
            // and we short circuit to just returning the times we could get and errors.
            _ => specTimeResults.targetTimes.pure[ResultT[F, *]],
            // If integration times were all successful, compute graphs with brightest target's times.
            (integrationTimes: AsterismIntegrationTimes) =>
              val graphRequest =
                buildAsterismGraphRequest(asterismRequest, figures)(integrationTimes)
              ResultT(spectroscopyGraphs(environment, cache, itc, config)(graphRequest)).map:
                (graphResult: SpectroscopyGraphsResult) =>
                  AsterismTimeAndGraphs.fromTimeAndGraphResults(integrationTimes, graphResult)
          )
          .bisequence
          .map: (timesOrGraphs: Either[AsterismIntegrationTimeOutcomes, AsterismTimeAndGraphs]) =>
            SpectroscopyTimeAndGraphsResult(
              specTimeResults.versions,
              AsterismTimesAndGraphsOutcomes(timesOrGraphs)
            )
      .value
      .onError: t =>
        Logger[F]
          .error(t):
            s"Error calculating spectroscopy integration time and graph for input: $asterismRequest"
      .toGraphQLErrors

  def apply[F[_]: Sync: Logger: Parallel: CustomSed.Resolver](
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
                RootEffect.computeEncodable("spectroscopyGraphs") { (_, env) =>
                  env
                    .getR[SpectroscopyGraphsInput]("input")
                    .flatMap(AsterismGraphRequest.fromInput)
                    .flatTraverse:
                      spectroscopyGraphs(environment, cache, itc, config)
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
                  "spectroscopyGraphs",
                  List(SpectroscopyGraphsInput.Binding("input", input))
                ) =>
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
