// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.option.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.itc.client.SpectroscopyInput
import lucuma.refined.*

trait CalibrationCountTestSupport extends ExecutionTestSupport:

  // Vary the science time with the requested exposure time and count.
  override def fakeItcSpectroscopyResultFor(input: SpectroscopyInput): Option[IntegrationTime] =
    input.parameters.mode.exposureTimeMode match
      case ExposureTimeMode.TimeAndCountMode(time, count, _) => IntegrationTime(time, count).some
      case ExposureTimeMode.SignalToNoiseMode(_, _)          =>
        IntegrationTime(5.minuteTimeSpan, 4.refined).some

  def calibrationCount(pid: Program.Id, oid: Observation.Id): IO[Int] =
    runObscalcUpdate(pid, oid) *>
    query(
      pi,
      s"""
        query {
          observation(observationId: "$oid") {
            execution { digest { value { estimate { calibrations { count } } } } }
          }
        }
      """
    ).map: json =>
      json.hcursor
        .downFields("observation", "execution", "digest", "value", "estimate", "calibrations", "count")
        .require[Int]

  def setTelluricType(oid: Observation.Id, tag: String): IO[Unit] =
    query(
      pi,
      s"""mutation {
        updateObservations(input: {
          WHERE: { id: { EQ: "$oid" } }
          SET: { observingMode: { flamingos2LongSlit: { telluricType: { tag: $tag } } } }
        }) {
          observations { id }
        }
      }"""
    ).void
