// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import io.circe.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.syntax.string.*

trait DaytimeCalibrationOps extends ReplaceSequenceOps:
  this: OdbSuite =>

  def createDaytimeCalibrationAs(user: User, pid: Program.Id, instrument: Instrument = Instrument.GmosNorth): IO[Observation.Id] =
    query(
      user  = user,
      query = s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson}
            SET: {
              observingMode: {
                daytimeCalibration: { instrument: ${instrument.tag.toScreamingSnakeCase} }
              }
            }
          }) {
            observation { id }
          }
        }
      """
    ).map(_.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id])

  // A GMOS North dark, the simplest day calibration step.
  def gmosNorthDarkStep(observeClass: String = "DAY_CAL", guiding: Option[String] = None): String =
    s"""
      {
        instrumentConfig: {
          exposure: { seconds: 10 }
          readout: {
            xBin: TWO
            yBin: TWO
            ampCount: TWELVE
            ampGain: LOW
            ampReadMode: SLOW
          }
          dtax: ZERO
          roi: FULL_FRAME
        }
        stepConfig: { dark: true }
        ${guiding.fold("")(g => s"telescopeConfig: { guiding: $g }")}
        observeClass: $observeClass
      }
    """

  def workflowQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: ${oid.asJson}) {
          workflow { value { state validationErrors { messages } } }
        }
      }
    """
