// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import cats.data.NonEmptyChain
import cats.syntax.all.*
import io.circe.Encoder
import io.circe.syntax.*
import lucuma.core.data.Zipper
import lucuma.core.math.Wavelength
import lucuma.core.util.NewType
import lucuma.core.util.TimeSpan

object AsterismIntegrationTimes extends NewType[Zipper[TargetIntegrationTime]]
type AsterismIntegrationTimes = AsterismIntegrationTimes.Type

object TargetIntegrationTimeOutcome extends NewType[Either[Error, TargetIntegrationTime]]:
  given (using Encoder[Wavelength], Encoder[TimeSpan]): Encoder[TargetIntegrationTimeOutcome] =
    _.value match
      case Left(e)  => e.asJson
      case Right(t) => t.asJson
type TargetIntegrationTimeOutcome = TargetIntegrationTimeOutcome.Type

object AsterismIntegrationTimeOutcomes extends NewType[NonEmptyChain[TargetIntegrationTimeOutcome]]:
  extension (a: AsterismIntegrationTimeOutcomes)
    // If there are errors, return them with their index.
    // If there are no errors, return an AsterismIntegrationTimes Zipper with the brightest target focused.
    def partitionErrors: Either[NonEmptyChain[(Error, Int)], AsterismIntegrationTimes] =
      a.value
        .traverse(_.value.toOption)
        .map: nec =>
          AsterismIntegrationTimes(Zipper.of(nec.head, nec.tail.toList*).focusMin)
        .toRight(collectErrors.get) // Should be safe to call get here, we know there are errors.

    // The brightest target is the one with the shortest exposure time.
    // Only returns the index of the brightest target if there are no errors.
    def brightestIndex: Option[Int] =
      partitionErrors.toOption.map(_.value.indexOfFocus)

    def brightest: Option[TargetIntegrationTime] =
      partitionErrors.toOption.map(_.value.focus)

    def collectErrors: Option[NonEmptyChain[(Error, Int)]] =
      NonEmptyChain.fromChain:
        a.value.map(_.value).zipWithIndex.collect { case (Left(e), i) => (e, i) }
type AsterismIntegrationTimeOutcomes = AsterismIntegrationTimeOutcomes.Type

object TargetGraphsOutcome extends NewType[Either[Error, TargetGraphsResult]]:
  given (using Encoder[Wavelength]): Encoder[TargetGraphsOutcome] =
    _.value match
      case Left(e)  => e.asJson
      case Right(t) => t.asJson
type TargetGraphsOutcome = TargetGraphsOutcome.Type

object AsterismTargetGraphsOutcomes extends NewType[NonEmptyChain[TargetGraphsOutcome]]:
  extension (a: AsterismTargetGraphsOutcomes)
    def collectErrors: Option[NonEmptyChain[(Error, Int)]] =
      NonEmptyChain.fromChain:
        a.value.map(_.value).zipWithIndex.collect { case (Left(e), i) => (e, i) }
type AsterismTargetGraphsOutcomes = AsterismTargetGraphsOutcomes.Type

object TargetTimeAndGraphsOutcome extends NewType[Either[Error, TargetTimeAndGraphs]]:
  given (using Encoder[Wavelength], Encoder[TimeSpan]): Encoder[TargetTimeAndGraphsOutcome] =
    _.value match
      case Left(e)  => e.asJson
      case Right(t) => t.asJson
type TargetTimeAndGraphsOutcome = TargetTimeAndGraphsOutcome.Type

object AsterismTimeAndGraphs extends NewType[NonEmptyChain[TargetTimeAndGraphsOutcome]]:
  def fromTimeAndGraphResults(
    specTimes:    AsterismIntegrationTimes,
    graphResults: SpectroscopyGraphsResult
  ): AsterismTimeAndGraphs =
    AsterismTimeAndGraphs:
      NonEmptyChain
        .fromNonEmptyList(specTimes.value.toNel)
        .zipWith(graphResults.targetGraphs.value):
          (integrationTime: TargetIntegrationTime, graphResult: TargetGraphsOutcome) =>
            TargetTimeAndGraphsOutcome:
              graphResult.value.map: (targetResult: TargetGraphsResult) =>
                // We discard the band from the graph result, it's already in the integration time.
                TargetTimeAndGraphs(integrationTime, targetResult.graphs)

  extension (a: AsterismTimeAndGraphs)
    // If there are no errors, return an Zipper with the brightest target focused.
    private def onlyIfNoErrors: Option[Zipper[TargetTimeAndGraphs]] =
      a.value
        .traverse(_.value.toOption)
        .map: nec =>
          Zipper.of(nec.head, nec.tail.toList*).focusMin

    // The brightest target is the one with the shortest exposure time.
    // Only returns the index of the brightest target if there are no errors.
    def brightestIndex: Option[Int] =
      onlyIfNoErrors.map(_.indexOfFocus)

    def brightest: Option[TargetTimeAndGraphs] =
      onlyIfNoErrors.map(_.focus)
type AsterismTimeAndGraphs = AsterismTimeAndGraphs.Type

object AsterismTimesAndGraphsOutcomes
    extends NewType[Either[AsterismIntegrationTimeOutcomes, AsterismTimeAndGraphs]]
type AsterismTimesAndGraphsOutcomes = AsterismTimesAndGraphsOutcomes.Type
