// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.syntax.*
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.odb.service.workflow.validator.GnirsSpectroscopyValidator

class observation_workflow_gnirs extends ExecutionTestSupportForGnirs:

  private def centralWavelength(nm: Int, seconds: BigDecimal): String =
    s"""
      {
        centralWavelength: { nanometers: $nm }
        exposureTimeMode: {
          timeAndCount: {
            time: { seconds: $seconds }
            count: 3
            at: { nanometers: $nm }
          }
        }
      }
    """

  // The default long slit configuration (D111, MIRROR, SHORT_BLUE, K filter at
  // 2200 nm, 30 s) with the given overrides.
  private def gnirsLongSlit(
    prism:            String       = "MIRROR",
    camera:           String       = "SHORT_BLUE",
    filter:           String       = "ORDER3",
    nm:               Int          = 2200,
    seconds:          BigDecimal   = 30,
    explicitReadMode: Option[String] = None,
    explicitDecker:   Option[String] = None,
    acqFilter:        Option[String] = None
  ): String =
    s"""
      gnirsLongSlit: {
        grating: D111
        prism: $prism
        camera: $camera
        fpu: LONG_SLIT_0_30
        filter: $filter
        centralWavelengths: [ ${centralWavelength(nm, seconds)} ]
        ${explicitReadMode.foldMap(m => s"explicitReadMode: $m")}
        ${explicitDecker.foldMap(d => s"explicitDecker: $d")}
        ${acqFilter.foldMap(f => s"acquisition: { explicitFilter: $f }")}
      }
    """

  // The default IFU configuration (D111, MIRROR, SHORT_BLUE, LR-IFU, K filter at
  // 2200 nm, 30 s) with the given overrides.
  private def gnirsIfu(
    explicitDecker: Option[String] = None
  ): String =
    s"""
      gnirsSpectroscopy: {
        grating: D111
        prism: MIRROR
        camera: SHORT_BLUE
        ifu: { fpu: LOW_RESOLUTION }
        filter: ORDER3
        centralWavelengths: [ ${centralWavelength(2200, 30)} ]
        ${explicitDecker.foldMap(d => s"explicitDecker: $d")}
      }
    """

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

  private def configurationValidations(mode: String): IO[List[(ObservationValidationCode, List[String])]] =
    for
      pid <- createProgram
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createObservationWithModeAs(pi, pid, List(tid), mode)
      _   <- runObscalcUpdate(pid, oid)
      vs  <- validations(oid)
    yield vs.filter: (code, _) =>
      code === ObservationValidationCode.ConfigurationError || code === ObservationValidationCode.ConfigurationWarning

  private def expectConfigurationValidations(mode: String, expected: (ObservationValidationCode, List[String])*): IO[Unit] =
    configurationValidations(mode).map(assertEquals(_, expected.toList))

  private def error(msgs: String*): (ObservationValidationCode, List[String]) =
    (ObservationValidationCode.ConfigurationError, msgs.toList)

  private def warning(msgs: String*): (ObservationValidationCode, List[String]) =
    (ObservationValidationCode.ConfigurationWarning, msgs.toList)

  private def nm(n: Int): Wavelength =
    Wavelength.fromIntNanometers(n).get

  test("default configuration has no configuration validations"):
    expectConfigurationValidations(gnirsLongSlit())

  test("cross-dispersed prism in the L band is an error"):
    expectConfigurationValidations(
      gnirsLongSlit(prism = "SXD", filter = "ORDER2", nm = 3300),
      error(GnirsSpectroscopyValidator.CrossDispersedThermal)
    )

  test("short blue camera with the LXD prism is an error"):
    expectConfigurationValidations(
      gnirsLongSlit(prism = "LXD", filter = "CROSS_DISPERSED"),
      error(GnirsSpectroscopyValidator.ShortBlueLxd)
    )

  test("red camera with a cross-dispersed prism is an error"):
    expectConfigurationValidations(
      gnirsLongSlit(camera = "SHORT_RED", prism = "SXD", filter = "CROSS_DISPERSED"),
      error(GnirsSpectroscopyValidator.RedCameraCrossDispersed)
    )

  test("filter that does not cover the central wavelength is a warning"):
    expectConfigurationValidations(
      gnirsLongSlit(filter = "ORDER4"),
      warning(GnirsSpectroscopyValidator.filterMismatch(GnirsFilter.Order4, nm(2200)))
    )

  test("explicit acquisition decker is a warning"):
    expectConfigurationValidations(
      gnirsLongSlit(explicitDecker = "ACQUISITION".some),
      warning(GnirsSpectroscopyValidator.DeckerAcquisitionMirror)
    )

  test("explicit decker for the wrong camera is a warning"):
    expectConfigurationValidations(
      gnirsLongSlit(explicitDecker = "LONG_CAM_LONG_SLIT".some),
      warning(GnirsSpectroscopyValidator.deckerMismatch(GnirsDecker.LongCamLongSlit, GnirsDecker.ShortCamLongSlit))
    )

  test("explicit decker matching the configuration is fine"):
    expectConfigurationValidations(gnirsLongSlit(explicitDecker = "SHORT_CAM_LONG_SLIT".some))

  test("IFU default configuration has no configuration validations"):
    expectConfigurationValidations(gnirsIfu())

  test("IFU explicit decker for the other resolution is a warning"):
    expectConfigurationValidations(
      gnirsIfu(explicitDecker = "HIGH_RESOLUTION_IFU".some),
      warning(GnirsSpectroscopyValidator.deckerMismatch(GnirsDecker.HighResolutionIfu, GnirsDecker.LowResolutionIfu))
    )

  test("IFU explicit decker matching the resolution is fine"):
    expectConfigurationValidations(gnirsIfu(explicitDecker = "LOW_RESOLUTION_IFU".some))

  test("explicit PAH acquisition filter below 2.5 µm is an error"):
    expectConfigurationValidations(
      gnirsLongSlit(acqFilter = "PAH".some),
      error(GnirsSpectroscopyValidator.BlueCameraAcquisitionFilter)
    )

  test("explicit J acquisition filter above 2.5 µm is an error"):
    expectConfigurationValidations(
      gnirsLongSlit(camera = "SHORT_RED", filter = "ORDER2", nm = 3300, acqFilter = "ORDER5".some),
      error(GnirsSpectroscopyValidator.RedCameraAcquisitionFilter)
    )

  test("explicit H acquisition filter above 2.5 µm is fine"):
    expectConfigurationValidations(
      gnirsLongSlit(camera = "SHORT_RED", filter = "ORDER2", nm = 3300, acqFilter = "ORDER4".some)
    )

  test("explicit read mode with an exposure below its minimum is an error"):
    expectConfigurationValidations(
      gnirsLongSlit(seconds = 5, explicitReadMode = "VERY_FAINT".some),
      error(GnirsSpectroscopyValidator.exposureTooShort(GnirsReadMode.VeryFaint, nm(2200)))
    )

  test("explicit read mode with an unusually long exposure is a warning"):
    expectConfigurationValidations(
      gnirsLongSlit(explicitReadMode = "VERY_BRIGHT".some),
      warning(GnirsSpectroscopyValidator.exposureUnusuallyLong(GnirsReadMode.VeryBright, 1.secTimeSpan, nm(2200)))
    )

  test("a configuration warning does not hide an exposure error"):
    expectConfigurationValidations(
      gnirsLongSlit(filter = "ORDER4", seconds = 5, explicitReadMode = "VERY_FAINT".some),
      error(GnirsSpectroscopyValidator.exposureTooShort(GnirsReadMode.VeryFaint, nm(2200))),
      warning(GnirsSpectroscopyValidator.filterMismatch(GnirsFilter.Order4, nm(2200)))
    )

  test("automatic read mode never triggers exposure validations"):
    expectConfigurationValidations(gnirsLongSlit(seconds = 0.7))
