// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.util.TimeSpan

trait ExecutionTestSupportForGnirs extends ExecutionTestSupport:

  /**
   * Replace the explicit `alongSlit` telescope configs on a GNIRS LongSlit
   * observation. `entries` is a GraphQL fragment of
   * `[TelescopeConfigAlongSlitInput!]`, e.g.
   *   `"""[ { q: { arcseconds: -2 }, guiding: ENABLED }, ... ]"""`.
   */
  def setAlongSlitTelescopeConfigs(oid: Observation.Id, entries: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gnirsLongSlit: {
                  explicitTelescopeConfigs: { alongSlit: $entries }
                }
              }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  /** Replace the explicit `toSky` telescope configs on a GNIRS LongSlit observation. */
  def setToSkyTelescopeConfigs(oid: Observation.Id, entries: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gnirsLongSlit: {
                  explicitTelescopeConfigs: { toSky: $entries }
                }
              }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  /** Replace the science TimeAndCount ETM on a GNIRS LongSlit observation. */
  def setScienceTimeAndCount(oid: Observation.Id, seconds: BigDecimal, count: Int, atNm: BigDecimal): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gnirsLongSlit: {
                  exposureTimeMode: {
                    timeAndCount: {
                      time:  { seconds: $seconds }
                      count: $count
                      at:    { nanometers: $atNm }
                    }
                  }
                }
              }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void


  val GnirsAtomQuery: String =
    s"""
      description
      observeClass
      steps {
        instrumentConfig {
          exposure { seconds }
          coadds
          centralWavelength { nanometers }
          filter
          decker
          fpuSlit
          fpuOther
          acquisitionMirrorOut {
            prism
            grating
            wavelength { nanometers }
          }
          camera
          focusMotorSteps
          readMode
        }
        stepConfig {
          stepType
        }
        telescopeConfig {
          offset {
            p { arcseconds }
            q { arcseconds }
          }
          guiding
        }
        observeClass
        breakpoint
      }
    """

  def gnirsAcquisitionQuery(oid: Observation.Id, futureLimit: Option[Int] = None): String =
    executionConfigQuery(oid, "gnirs", "acquisition", GnirsAtomQuery, futureLimit)

  def gnirsScienceQuery(oid: Observation.Id, futureLimit: Option[Int] = None): String =
    executionConfigQuery(oid, "gnirs", "science", GnirsAtomQuery, futureLimit)

  /** Set the acquisition T+C ETM on a GNIRS LongSlit observation. */
  def setAcquisitionTimeAndCount(oid: Observation.Id, seconds: BigDecimal, count: Int, atNm: BigDecimal): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gnirsLongSlit: {
                  acquisition: {
                    exposureTimeMode: {
                      timeAndCount: {
                        time:  { seconds: $seconds }
                        count: $count
                        at:    { nanometers: $atNm }
                      }
                    }
                  }
                }
              }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  /** Set the explicit acquisition type on a GNIRS LongSlit observation. */
  def setAcquisitionType(oid: Observation.Id, acqType: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gnirsLongSlit: {
                  acquisition: {
                    explicitAcquisitionType: $acqType
                  }
                }
              }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  /** Set the acquisition sky offset on a GNIRS LongSlit observation. */
  def setAcquisitionSkyOffset(oid: Observation.Id, pArcsec: BigDecimal, qArcsec: BigDecimal): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gnirsLongSlit: {
                  acquisition: {
                    skyOffset: {
                      p: { arcseconds: $pArcsec }
                      q: { arcseconds: $qArcsec }
                    }
                  }
                }
              }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  /**
   * Set the explicit FAINT acquisition type together with its required sky offset
   * (the two must be set in the same input — FAINT requires a sky offset, and a sky
   * offset is only valid with FAINT).
   */
  def setAcquisitionFaint(oid: Observation.Id, pArcsec: BigDecimal, qArcsec: BigDecimal): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gnirsLongSlit: {
                  acquisition: {
                    explicitAcquisitionType: FAINT
                    skyOffset: { p: { arcseconds: $pArcsec }, q: { arcseconds: $qArcsec } }
                  }
                }
              }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  /** Set the camera on a GNIRS LongSlit observation. */
  def setCamera(oid: Observation.Id, camera: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gnirsLongSlit: {
                  camera: $camera
                }
              }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  /** Set the explicit acquisition filter on a GNIRS LongSlit observation. */
  def setAcquisitionFilter(oid: Observation.Id, filter: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: {
              observingMode: {
                gnirsLongSlit: {
                  acquisition: {
                    explicitFilter: $filter
                  }
                }
              }
            }
            WHERE: { id: { EQ: "$oid" } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  /**
   * Description of a single GNIRS dynamic config worth of fields, shared by
   * every science step in the cycle (only the telescope offset varies).
   */
  case class GnirsDynamicSnapshot(
    exposureTime:        TimeSpan,
    coadds:              Int,
    centralWavelengthNm: BigDecimal,
    filter:              String,
    decker:              String,
    fpuSlit:             Option[String],
    fpuOther:            Option[String],
    prism:               Option[String],
    grating:             Option[String],
    mirrorWavelengthNm:  Option[BigDecimal],
    camera:              String,
    focus:               Option[Int],
    readMode:            String
  ):
    def acquisitionMirrorOut: Json =
      (prism, grating, mirrorWavelengthNm) match
        case (Some(p), Some(g), Some(w)) =>
          json"""{
            "prism":      ${p.asJson},
            "grating":    ${g.asJson},
            "wavelength": { "nanometers": ${w.asJson} }
          }"""
        case _ => Json.Null

  protected def gnirsExpectedScience(
    cfg: GnirsDynamicSnapshot,
    p:   BigDecimal,
    q:   BigDecimal,
    g:   StepGuideState
  ): Json =
    val tc = TelescopeConfig(
      Offset(
        Offset.P.signedDecimalArcseconds.reverseGet(p),
        Offset.Q.signedDecimalArcseconds.reverseGet(q)
      ),
      g
    )
    json"""
      {
        "instrumentConfig": {
          "exposure":             { "seconds": ${cfg.exposureTime.toSeconds} },
          "coadds":               ${cfg.coadds.asJson},
          "centralWavelength":    { "nanometers": ${cfg.centralWavelengthNm.asJson} },
          "filter":               ${cfg.filter.asJson},
          "decker":               ${cfg.decker.asJson},
          "fpuSlit":              ${cfg.fpuSlit.asJson},
          "fpuOther":             ${cfg.fpuOther.asJson},
          "acquisitionMirrorOut": ${cfg.acquisitionMirrorOut},
          "camera":               ${cfg.camera.asJson},
          "focusMotorSteps":      ${cfg.focus.asJson},
          "readMode":             ${cfg.readMode.asJson}
        },
        "stepConfig": { "stepType": "SCIENCE" },
        "telescopeConfig": ${expectedTelescopeConfig(tc)},
        "observeClass": "SCIENCE",
        "breakpoint": "DISABLED"
      }
    """

  protected def gnirsExpectedScienceAtom(
    cfg:     GnirsDynamicSnapshot,
    offsets: (BigDecimal, BigDecimal, StepGuideState)*
  ): Json =
    val sciSteps =
      offsets.toList.map((p, q, g) => gnirsExpectedScience(cfg, p, q, g))

    Json.obj(
      "description"  -> "Science Cycle".asJson,
      "observeClass" -> "SCIENCE".asJson,
      "steps"        -> sciSteps.asJson
    )
