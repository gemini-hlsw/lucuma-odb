// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosDtax
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Angle
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.sequence.data.Completion
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Gmos.GratingConfigKey
import lucuma.odb.smartgcal.data.Gmos.TableKey
import lucuma.odb.smartgcal.data.Gmos.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import natchez.Trace.Implicits.noop
import skunk.Session

trait ExecutionTestSupport extends OdbSuite with ObservingModeSetupOperations {

  val pi: User   = TestUsers.Standard.pi(1, 30)
  val user: User = TestUsers.service(3)

  override val validUsers: List[User] =
    List(pi, user)

  val createProgram: IO[Program.Id] =
    createProgramAs(user, "Sequence Testing")

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some { s =>
    val tableRow1: TableRow.North =
      TableRow(
        PosLong.unsafeFrom(1),
        TableKey(
          GratingConfigKey(
            GmosNorthGrating.R831_G5302,
            GmosGratingOrder.One,
            BoundedInterval.unsafeOpenUpper(Wavelength.Min, Wavelength.Max)
          ).some,
          GmosNorthFilter.RPrime.some,
          GmosNorthFpu.LongSlit_0_50.some,
          GmosXBinning.One,
          GmosYBinning.Two,
          GmosAmpGain.Low
        ),
        SmartGcalValue(
          Gcal(
            Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W),
            GcalFilter.Gmos,
            GcalDiffuser.Ir,
            GcalShutter.Open
          ),
          GcalBaselineType.Night,
          PosInt.unsafeFrom(1),
          LegacyInstrumentConfig(
            TimeSpan.unsafeFromMicroseconds(1_000_000L)
          )
        )
      )
    val tableRow2: TableRow.North =
      TableRow(
        PosLong.unsafeFrom(1),
        TableKey(
          GratingConfigKey(
            GmosNorthGrating.R831_G5302,
            GmosGratingOrder.One,
            BoundedInterval.unsafeOpenUpper(Wavelength.Min, Wavelength.Max)
          ).some,
          GmosNorthFilter.RPrime.some,
          GmosNorthFpu.LongSlit_5_00.some,
          GmosXBinning.One,
          GmosYBinning.Two,
          GmosAmpGain.Low
        ),
        SmartGcalValue(
          Gcal(
            Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W),
            GcalFilter.Gmos,
            GcalDiffuser.Ir,
            GcalShutter.Open
          ),
          GcalBaselineType.Night,
          PosInt.unsafeFrom(1),
          LegacyInstrumentConfig(
            TimeSpan.unsafeFromMicroseconds(1_000_000L)
          )
        )
      )

    Enums.load(s).flatMap { e =>
      val services = Services.forUser(pi /* doesn't matter*/, e)(s)
      services.transactionally {
        services.smartGcalService.insertGmosNorth(1, tableRow1) *>
        services.smartGcalService.insertGmosNorth(2, tableRow2)
      }
    }
  }

  val GmosInstrumentConfigQuery: String =
    s"""
      instrumentConfig {
        exposure {
          seconds
        }
        exposure {
          seconds
        }
        readout {
          xBin
          yBin
          ampCount
          ampGain
          ampReadMode
        }
        dtax
        roi
        gratingConfig {
          grating
          order
          wavelength {
            nanometers
          }
        }
        filter
        fpu {
          builtin
        }
      }
    """

  val StepConfigScienceQuery: String =
    s"""
      stepConfig {
        ... on Science {
          offset {
            p { arcseconds }
            q { arcseconds }
          }
        }
      }
    """

  val GmosScienceAtomQuery: String =
    s"""
      steps {
        $GmosInstrumentConfigQuery
        $StepConfigScienceQuery
      }
    """

  def addEndStepEvent(sid: Step.Id): IO[Unit] = {
    val q = s"""
      mutation {
        addStepEvent(input: {
          stepId: "$sid",
          stepStage: END_STEP
        }) {
          event {
            step {
              id
            }
          }
        }
      }
    """

    query(user, q).void
  }

  def addDatasetEvent(did: Dataset.Id, stage: DatasetStage): IO[Unit] = {
    val q = s"""
      mutation {
        addDatasetEvent(input: {
          datasetId:    "$did",
          datasetStage: ${stage.tag.toUpperCase}
        }) {
          event {
            id
          }
        }
      }
    """

    query(user, q).void
  }

  def setQaState(did: Dataset.Id, qa: DatasetQaState): IO[Unit] = {
    val q = s"""
        mutation {
          updateDatasets(input: {
            SET: {
              qaState: ${qa.tag.toUpperCase}
            },
            WHERE: {
              id: { EQ: "$did" }
            }
          }) {
            datasets {
              id
            }
          }
        }
    """

    query(user, q).void
  }

  val Seconds01 = TimeSpan.unsafeFromMicroseconds( 1_000_000L)
  val Seconds10 = TimeSpan.unsafeFromMicroseconds(10_000_000L)
  val Seconds20 = TimeSpan.unsafeFromMicroseconds(20_000_000L)
  val Seconds30 = TimeSpan.unsafeFromMicroseconds(30_000_000L)

  val GmosNorthAcq0: GmosNorth =
    GmosNorth(
      Seconds10,
      GmosCcdMode(GmosXBinning.Two, GmosYBinning.Two, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Fast),
      GmosDtax.Zero,
      GmosRoi.Ccd2,
      none,
      GmosNorthFilter.GPrime.some,
      none
    )

  val GmosNorthAcq0Json: Json =
    json"""
      {
        "instrumentConfig": {
          "exposure": { "seconds": 10.000000 },
          "readout": {
            "xBin": "TWO",
            "yBin": "TWO",
            "ampCount": "TWELVE",
            "ampGain": "LOW",
            "ampReadMode": "FAST"
          },
          "dtax": "ZERO",
          "roi": "CCD2",
          "gratingConfig": null,
          "filter": "G_PRIME",
          "fpu": null
        }
      }
    """

  val GmosNorthAcq1: GmosNorth =
    GmosNorth(
      Seconds20,
      GmosCcdMode(GmosXBinning.One, GmosYBinning.One, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Fast),
      GmosDtax.Zero,
      GmosRoi.CentralStamp,
      none,
      GmosNorthFilter.GPrime.some,
      GmosFpuMask.Builtin(GmosNorthFpu.LongSlit_0_50).some
    )

  val GmosNorthAcq1Json: Json =
    json"""
      {
        "instrumentConfig": {
          "exposure": { "seconds": 20.000000 },
          "readout": {
            "xBin": "ONE",
            "yBin": "ONE",
            "ampCount": "TWELVE",
            "ampGain": "LOW",
            "ampReadMode": "FAST"
          },
          "dtax": "ZERO",
          "roi": "CENTRAL_STAMP",
          "gratingConfig": null,
          "filter": "G_PRIME",
          "fpu": { "builtin": "LONG_SLIT_0_50" }
        }
      }
    """

  val GmosNorthAcq2: GmosNorth =
    GmosNorth(
      Seconds30,
      GmosCcdMode(GmosXBinning.One, GmosYBinning.One, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Fast),
      GmosDtax.Zero,
      GmosRoi.CentralStamp,
      none,
      GmosNorthFilter.GPrime.some,
      GmosFpuMask.Builtin(GmosNorthFpu.LongSlit_0_50).some
    )

  val GmosNorthAcq2Json: Json =
    json"""
      {
        "instrumentConfig": {
          "exposure": { "seconds": 30.000000 },
          "readout": {
            "xBin": "ONE",
            "yBin": "ONE",
            "ampCount": "TWELVE",
            "ampGain": "LOW",
            "ampReadMode": "FAST"
          },
          "dtax": "ZERO",
          "roi": "CENTRAL_STAMP",
          "gratingConfig": null,
          "filter": "G_PRIME",
          "fpu": { "builtin": "LONG_SLIT_0_50" }
        }
      }
    """

  val GmosNorthScience0: GmosNorth =
    GmosNorth(
      Seconds10,
      GmosCcdMode(GmosXBinning.One, GmosYBinning.Two, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Slow),
      GmosDtax.Zero,
      GmosRoi.FullFrame,
      GmosGratingConfig.North(GmosNorthGrating.R831_G5302, GmosGratingOrder.One, Wavelength.decimalNanometers.unsafeGet(BigDecimal("500.0"))).some,
      GmosNorthFilter.RPrime.some,
      GmosFpuMask.Builtin(GmosNorthFpu.LongSlit_0_50).some
    )

  val GmosNorthScience5: GmosNorth =
    GmosNorthScience0.copy(
      gratingConfig = GmosNorthScience0.gratingConfig.map(_.copy(
        wavelength = Wavelength.decimalNanometers.unsafeGet(BigDecimal("505.0"))
      ))
    )

  val GmosNorthFlat0 = GmosNorthScience0.copy(exposure = Seconds01)
  val GmosNorthFlat5 = GmosNorthScience5.copy(exposure = Seconds01)

  val P00Q00 = Offset.Zero
  val P10Q00 = Offset.microarcseconds.reverseGet((10_000_000L, 0L))
  val P00Q15 = Offset.microarcseconds.reverseGet((0L, 15_000_000L))

  val ScienceP00Q00 = StepConfig.Science(P00Q00, StepGuideState.Enabled)
  val ScienceP10Q00 = StepConfig.Science(P10Q00, StepGuideState.Enabled)
  val ScienceP00Q15 = StepConfig.Science(P00Q15, StepGuideState.Enabled)
  val Flat          = StepConfig.Gcal(Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W), GcalFilter.Gmos, GcalDiffuser.Ir, GcalShutter.Open)

  def stepConfigScienceJson(o: Offset): Json =
    json"""
      {
        "stepConfig": {
          "offset": {
            "p": { "arcseconds": ${Angle.signedDecimalArcseconds.get(o.p.toAngle)} },
            "q": { "arcseconds": ${Angle.signedDecimalArcseconds.get(o.q.toAngle)} }
          }
        }
      }
    """

  val StepConfigScienceP00Q00Json = stepConfigScienceJson(P00Q00)
  val StepConfigScienceP10Q00Json = stepConfigScienceJson(P10Q00)
  val StepConfigScienceP00Q15Json = stepConfigScienceJson(P00Q15)

  def scienceSingleAtomCompletionState[D](idBase: Int, steps: Completion.StepMatch[D]*): Completion.State[D] =
    Completion.State(
      Completion.Sequence.idBase(idBase),
      Completion.Sequence(idBase, Completion.AtomMap.from(steps.toList -> PosInt.unsafeFrom(1)))
    )

  def genGmosNorthSequence(oid: Observation.Id, seqType: SequenceType, futureLimit: Int): IO[List[Atom.Id]] =
    query(
      user = user,
      query = s"""
        query {
          observation(observationId: "$oid") {
            execution {
              config(futureLimit: $futureLimit) {
                gmosNorth {
                  ${seqType.tag} {
                    nextAtom {
                      id
                    }
                    possibleFuture {
                      id
                    }
                  }
                }
              }
            }
          }
        }
      """
    ).map { json =>
      val sci = json.hcursor.downFields("observation", "execution", "config", "gmosNorth", seqType.tag)
      val n   = sci.downFields("nextAtom", "id").require[Atom.Id]
      val fs  = sci.downFields("possibleFuture").values.toList.flatMap(_.toList.map(_.hcursor.downField("id").require[Atom.Id]))
      n :: fs
    }

}
