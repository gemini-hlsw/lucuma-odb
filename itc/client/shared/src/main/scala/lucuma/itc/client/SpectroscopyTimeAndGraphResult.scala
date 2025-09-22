// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.client

import cats.Eq
import cats.Order
import cats.data.NonEmptyChain
import cats.derived.*
import cats.syntax.all.*
import io.circe.Decoder
import lucuma.core.data.Zipper
import lucuma.core.util.NewType
import lucuma.itc.AsterismIntegrationTimeOutcomes
import lucuma.itc.Error
import lucuma.itc.ItcVersions
import lucuma.itc.TargetIntegrationTime
import lucuma.itc.client.json.decoders.given

case class TargetTimeAndGraphsResult(
  integrationTime: TargetIntegrationTime,
  graphs:          TargetGraphs
) derives Decoder:
  export integrationTime.{ccds as integrationTimeCcds, *}
  export graphs.{ccds as graphCcds, *}

object TargetTimeAndGraphsResult:
  given Order[TargetTimeAndGraphsResult] = Order.by(_.integrationTime)

object TargetTimeAndGraphsResultOutcome extends NewType[Either[Error, TargetTimeAndGraphsResult]]:
  given Decoder[TargetTimeAndGraphsResultOutcome] =
    Decoder[TargetTimeAndGraphsResult]
      .map(_.asRight)
      .or(Decoder[Error].map(_.asLeft))
      .map(TargetTimeAndGraphsResultOutcome(_))
type TargetTimeAndGraphsResultOutcome = TargetTimeAndGraphsResultOutcome.Type

object AsterismTimeAndGraphsResult extends NewType[NonEmptyChain[TargetTimeAndGraphsResultOutcome]]:
  extension (a: AsterismTimeAndGraphsResult)
    // If there are no errors, return an Zipper with the brightest target focused.
    private def onlyIfNoErrors: Option[Zipper[TargetTimeAndGraphsResult]] =
      a.value
        .traverse(_.value.toOption)
        .map: nec =>
          Zipper.of(nec.head, nec.tail.toList*).focusMin

    // The brightest target is the one with the shortest exposure time.
    // Only returns the index of the brightest target if there are no errors.
    def brightestIndex: Option[Int] =
      onlyIfNoErrors.map(_.indexOfFocus)

    def brightest: Option[TargetTimeAndGraphsResult] =
      onlyIfNoErrors.map(_.focus)
type AsterismTimeAndGraphsResult = AsterismTimeAndGraphsResult.Type

object AsterismTimesAndGraphsResultOutcomes
    extends NewType[Either[AsterismIntegrationTimeOutcomes, AsterismTimeAndGraphsResult]]
type AsterismTimesAndGraphsResultOutcomes = AsterismTimesAndGraphsResultOutcomes.Type

case class SpectroscopyIntegrationTimeAndGraphsResult(
  versions:       ItcVersions,
  graphsOrTimes:  AsterismTimesAndGraphsResultOutcomes,
  brightestIndex: Option[Int]
) derives Eq
