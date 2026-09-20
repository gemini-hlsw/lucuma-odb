// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.syntax.*
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.model.Observation
import lucuma.odb.service.workflow.validator.AltairValidator

/**
 * GNIRS observations carrying an Altair configuration, checked against
 * `AltairValidator`.  Every observation here starts from the default long
 * slit configuration (D111 / MIRROR / SHORT_BLUE / ORDER3 at 2200 nm), which
 * `observation_workflow_gnirs` already establishes has no configuration
 * validations of its own, so any validation seen below comes from Altair.
 */
class observation_workflow_altair extends ExecutionTestSupportForGnirs:

  private def centralWavelength(nm: Int): String =
    s"{ centralWavelength: { nanometers: $nm } }"

  private def gnirsLongSlit(
    camera: String = "SHORT_BLUE",
    filter: String = "ORDER3",
    nm:     Int    = 2200
  ): String =
    s"""
      gnirsLongSlit: {
        grating: D111
        prism: MIRROR
        camera: $camera
        fpu: LONG_SLIT_0_30
        filter: $filter
        centralWavelengths: [ ${centralWavelength(nm)} ]
      }
    """

  private def setConstraints(oid: Observation.Id, imageQuality: Option[String], cloudExtinction: Option[String]): IO[Unit] =
    if imageQuality.isEmpty && cloudExtinction.isEmpty then IO.unit
    else
      query(
        pi,
        s"""
          mutation {
            updateObservations(input: {
              SET: {
                constraintSet: {
                  ${imageQuality.foldMap(v => s"imageQuality: $v")}
                  ${cloudExtinction.foldMap(v => s"cloudExtinction: $v")}
                }
              }
              WHERE: { id: { EQ: "$oid" } }
            }) {
              observations { id }
            }
          }
        """
      ).void

  private def setAltair(oid: Observation.Id, altair: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              targetEnvironment: { altair: $altair }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  private def validations(oid: Observation.Id): IO[List[(ObservationValidationCode, List[String])]] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            workflow {
              value {
                validationErrors {
                  code
                  messages
                }
              }
            }
          }
        }
      """
    ).map: json =>
      json
        .hcursor
        .downFields("observation", "workflow", "value", "validationErrors")
        .values
        .toList
        .flatten
        .map: v =>
          (
            v.hcursor.downField("code").require[ObservationValidationCode],
            v.hcursor.downField("messages").require[List[String]]
          )

  private def error(msgs: String*): (ObservationValidationCode, List[String]) =
    (ObservationValidationCode.ConfigurationError, msgs.toList)

  private def warning(msgs: String*): (ObservationValidationCode, List[String]) =
    (ObservationValidationCode.ConfigurationWarning, msgs.toList)

  // Altair needs a guide star, which every test but the dedicated ones supplies so that
  // its own rule is what the expectations show.
  private val GuideStarName: String =
    "Gaia DR3 3219118090462918016"

  private def expectAltairValidations(
    mode:            String,
    altair:          String,
    imageQuality:    Option[String] = None,
    cloudExtinction: Option[String] = None,
    guideStar:       Boolean        = true
  )(expected: (ObservationValidationCode, List[String])*): IO[Unit] =
    for
      pid <- createProgram
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationWithModeAs(pi, pid, List(tid), mode)
      _   <- setConstraints(oid, imageQuality, cloudExtinction)
      _   <- setAltair(oid, altair)
      _   <- IO.whenA(guideStar)(setGuideTargetName(pi, oid, GuideStarName.some))
      _   <- runObscalcUpdate(pid, oid)
      vs  <- validations(oid)
    yield assertEquals(
      vs.filter { case (code, _) => code === ObservationValidationCode.ConfigurationError || code === ObservationValidationCode.ConfigurationWarning },
      expected.toList
    )

  test("Altair LGS+P1 on a long slit observation is a warning"):
    expectAltairValidations(
      gnirsLongSlit(),
      altair          = "{ mode: LGS_P1 }",
      imageQuality    = "POINT_THREE".some,
      cloudExtinction = "ZERO".some
    )(warning(AltairValidator.LgsP1LongSlitMessage))

  test("Altair LGS is an error when cloud extinction is 0.1 mag or worse"):
    expectAltairValidations(
      gnirsLongSlit(),
      altair          = "{ mode: LGS }",
      imageQuality    = "POINT_THREE".some,
      cloudExtinction = "POINT_ONE".some
    )(error(AltairValidator.LgsConditionsMessage))

  test("Altair LGS+P1 without a selected guide star is an error"):
    expectAltairValidations(
      gnirsLongSlit(),
      altair          = "{ mode: LGS_P1 }",
      imageQuality    = "POINT_THREE".some,
      cloudExtinction = "ZERO".some,
      guideStar       = false
    )(error(AltairValidator.MissingGuideStarMessage), warning(AltairValidator.LgsP1LongSlitMessage))

  test("Altair NGS at 4.5 µm is an error"):
    expectAltairValidations(
      gnirsLongSlit(camera = "SHORT_RED", filter = "ORDER1", nm = 4500),
      altair = "{ mode: NGS }"
    )(error(AltairValidator.WavelengthTooLongMessage))

  test("Altair NGS at 1.2 µm is a warning"):
    expectAltairValidations(
      gnirsLongSlit(filter = "ORDER5", nm = 1200),
      altair = "{ mode: NGS }"
    )(warning(AltairValidator.WavelengthTooShortMessage))

  test("Altair NGS at 2.2 µm with good conditions has no Altair validations"):
    expectAltairValidations(
      gnirsLongSlit(),
      altair          = "{ mode: NGS }",
      imageQuality    = "POINT_THREE".some,
      cloudExtinction = "ZERO".some
    )()

  test("Altair NGS without a selected guide star is an error"):
    expectAltairValidations(
      gnirsLongSlit(),
      altair          = "{ mode: NGS }",
      imageQuality    = "POINT_THREE".some,
      cloudExtinction = "ZERO".some,
      guideStar       = false
    )(error(AltairValidator.MissingGuideStarMessage))
