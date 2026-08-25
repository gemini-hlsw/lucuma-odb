// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.NonEmptyList
import cats.effect.IO
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import munit.TestOptions

import ObservingModeSetupOperations.*

trait ObservingModeSetupOperations extends DatabaseOperations { this: OdbSuite =>

  private def formatExplicitSpatialOffsetsInput(arcsecs: List[Int]): String =
    arcsecs.map(a => s"{ arcseconds: $a }").mkString("explicitSpatialOffsets: [", ", ", "]")

  def createFlamingos2LongSlitObservationAs(
    user:         User,
    pid:          Program.Id,
    tids:         List[Target.Id]
  ): IO[Observation.Id] =
    createObservationWithModeAs(
      user,
      pid,
      tids,
      s"""
        flamingos2LongSlit: {
          disperser: R1200_JH
          filter: JH
          fpu: LONG_SLIT_1
        }
      """
    )

  def createFlamingos2MosObservationAs(
    user: User,
    pid:  Program.Id,
    tids: List[Target.Id]
  ): IO[Observation.Id] =
    createObservationWithModeAs(
      user,
      pid,
      tids,
      s"""
        flamingos2Mos: {
          disperser: R1200_JH
          filter: JH
          customMask: { slitWidth: CUSTOM_WIDTH_2_PIX }
        }
      """
    )

  def createGhostIfuObservationAs(
    user:           User,
    pid:            Program.Id,
    tids:           List[Target.Id],
    resolutionMode: String         = "STANDARD",
    redReadMode:    Option[String] = None,
    ifu1Agitator:   Option[String] = None
  ): IO[Observation.Id] =
    createObservationWithModeAs(
      user,
      pid,
      tids,
      s"""
        ghostIfu: {
          stepCount: 1
          resolutionMode: $resolutionMode
          red: {
            exposureTimeMode: {
              timeAndCount: {
                time: { seconds: 1 }
                count: 1
                at: { nanometers: 500 }
              }
            }
            ${redReadMode.fold("")(m => s"explicitReadMode: $m")}
          }
          blue: {
            exposureTimeMode: {
              timeAndCount: {
                time: { seconds: 1 }
                count: 1
                at: { nanometers: 500 }
              }
            }
          }
          ${ifu1Agitator.fold("")(a => s"explicitIfu1Agitator: $a")}
        }
      """
    )

  def createGmosNorthLongSlitObservationAs(
    user:         User,
    pid:          Program.Id,
    tids:         List[Target.Id],
    offsetArcsec: Option[List[Int]] = None
  ): IO[Observation.Id] =
    createObservationWithModeAs(
      user,
      pid,
      tids,
      s"""
        gmosNorthLongSlit: {
          grating: R831_G5302
          filter: R_PRIME
          fpu: LONG_SLIT_0_50
          centralWavelength: {
            nanometers: 500
          }
          explicitYBin: TWO
          ${offsetArcsec.fold("")(formatExplicitSpatialOffsetsInput)}
        }
      """
    )

  def createGmosSouthLongSlitObservationAs(
    user:         User,
    pid:          Program.Id,
    tids:         List[Target.Id]
  ): IO[Observation.Id] =
    createObservationWithModeAs(
      user,
      pid,
      tids,
      """
        gmosSouthLongSlit: {
          grating: R600_G5324,
          filter: R_PRIME,
          fpu: LONG_SLIT_0_50,
          centralWavelength: {
            nanometers: 500
          },
          explicitYBin: TWO
        }
      """
    )

  def createGmosNorthMosObservationAs(
    user:         User,
    pid:          Program.Id,
    tids:         List[Target.Id],
    offsetArcsec: Option[List[Int]] = None
  ): IO[Observation.Id] =
    createObservationWithModeAs(
      user,
      pid,
      tids,
      s"""
        gmosNorthMos: {
          grating: R831_G5302
          filter: R_PRIME
          customMask: {
            slitWidth: CUSTOM_WIDTH_0_50
          }
          centralWavelength: {
            nanometers: 500
          }
          explicitYBin: TWO
          ${offsetArcsec.fold("")(formatExplicitSpatialOffsetsInput)}
        }
      """
    )

  def createGmosSouthMosObservationAs(
    user: User,
    pid:  Program.Id,
    tids: List[Target.Id]
  ): IO[Observation.Id] =
    createObservationWithModeAs(
      user,
      pid,
      tids,
      """
        gmosSouthMos: {
          grating: R600_G5324,
          filter: R_PRIME,
          customMask: {
            slitWidth: CUSTOM_WIDTH_0_50
          },
          centralWavelength: {
            nanometers: 500
          },
          explicitYBin: TWO
        }
      """
    )

  def createObservationWithModeQuery(
    pid:  Program.Id,
    tids: List[Target.Id],
    mode: String
  ): String =
    s"""
      mutation {
        createObservation(input: {
          programId: ${pid.asJson},
          SET: {
            $ConstraintSet,
            targetEnvironment: {
              asterism: ${tids.asJson}
            },
            $SpectroscopyScienceRequirements,
            observingMode: {
              $mode
            }
          }
        }) {
          observation {
            id
          }
        }
      }
    """

  /**
   * An observation holding an opportunity target derives a ToO activation other
   * than NONE, and is only coherent if its proposal allows that much disruption
   * (otherwise the workflow flags it `Unapproved`).  Fixtures built from an
   * opportunity target are not trying to exercise that rule, so raise the ceiling
   * far enough to permit whatever they derive.  Nothing is set on the observation
   * itself: the activation follows from the asterism.
   *
   * The ceiling has to be written directly: it is normally derived from the
   * program's observations and frozen when the proposal is accepted, and several
   * fixtures accept the proposal before the observation exists, which would
   * freeze it at NONE.
   *
   * It goes to the *top* of the ladder rather than a middle rung.  This used to
   * write 'rapid', which is one short: an observation whose mode is INTERRUPTING
   * derives INTERRUPTING, exceeds the ceiling, and lands `Unapproved` -- so it can
   * never be offered `Ready`, and any fixture built at that mode is untriggerable
   * for a reason that has nothing to do with what the test is about.
   */
  private def raiseTooCeilingForOpportunityTargets(
    oid:  Observation.Id,
    tids: List[Target.Id]
  ): IO[Unit] =
    import skunk.syntax.all.*
    import skunk.codec.numeric.int8
    import lucuma.odb.util.Codecs.nel
    import lucuma.odb.util.Codecs.observation_id
    import lucuma.odb.util.Codecs.target_id

    def hasOpportunityTarget(tns: NonEmptyList[Target.Id]): IO[Boolean] =
      val enc = target_id.nel(tns)
      session.use: s =>
        s.prepareR(sql"SELECT count(*) FROM t_target WHERE c_type = 'opportunity' AND c_target_id IN ($enc)".query(int8))
          .use(_.unique(tns).map(_ > 0L))

    val raiseCeiling: IO[Unit] =
      session.use: s =>
        s.prepareR(
          sql"""
            UPDATE t_proposal
            SET c_too_activation = 'interrupting'
            WHERE c_program_id = (
              SELECT c_program_id FROM t_observation WHERE c_observation_id = $observation_id
            )
          """.command
        ).use(_.execute(oid).void)

    NonEmptyList.fromList(tids).fold(IO.unit): tns =>
      hasOpportunityTarget(tns).flatMap(IO.whenA(_)(raiseCeiling))

  def createObservationWithModeAs(
    user:         User,
    pid:          Program.Id,
    tids:         List[Target.Id],
    mode:         String,
  ): IO[Observation.Id] =
    query(
      user  = user,
      query = createObservationWithModeQuery(pid, tids, mode),
    ).map { json =>
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }.flatTap(raiseTooCeilingForOpportunityTargets(_, tids))

  def createObservationWithNoModeAs(
    user:         User,
    pid:          Program.Id,
    tid:          Target.Id,
  ): IO[Observation.Id] =
    query(
      user  = user,
      query =
      s"""
        mutation {
          createObservation(input: {
            programId: ${pid.asJson},
            SET: {
              $ConstraintSet,
              targetEnvironment: {
                asterism: ${List(tid).asJson}
              },
              $SpectroscopyScienceRequirements,
            }
          }) {
            observation {
              id
            }
          }
        }
      """
    ).map { json =>
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }

  enum TargetType:
    case Sidereal, Nonsidereal, Opportunity

  /** Create multiple tests that take an injected Target constructor. */
  def testWithTargetTypes(
    name: String | TestOptions,
    ctors: Map[TargetType, (User, Program.Id) => IO[Target.Id]] =
      Map(
        TargetType.Sidereal    -> ((u, p) => createTargetWithProfileAs(u, p)),
        TargetType.Opportunity -> ((u, p) => createOpportunityTargetAs(u, p)),
        TargetType.Nonsidereal -> ((u, p) => createNonsiderealTargetAs(u, p)),
      )
  )(body: (TargetType, (User, Program.Id) => IO[Target.Id]) => Any) =
    ctors.foreach: (tt, fun) =>
      val prefix = s"[$tt]".padTo(13, ' ')
      val ops = name match
        case s: String => TestOptions(s"$prefix $name")
        case o: TestOptions => o.withName(s"$prefix ${o.name}")
      test(ops)(body(tt, fun))

}

object ObservingModeSetupOperations {

  val ConstraintSet: String =
    """
      constraintSet: {
        cloudExtinction: POINT_FIVE,
        imageQuality: ONE_POINT_ZERO,
        skyBackground: DARK
      }
    """

  val SpectroscopyScienceRequirements: String =
    """
      scienceRequirements: {
        exposureTimeMode: {
          signalToNoise: {
            value: 100.0,
            at: { nanometers: 500 }
          }
        },
        spectroscopy: {
          wavelength: {
            nanometers: 500
          },
          resolution: 100,
          wavelengthCoverage: {
            nanometers: 20
          },
          focalPlane: SINGLE_SLIT,
          focalPlaneAngle: {
            microarcseconds: 0
          }
        }
      }
    """

}
