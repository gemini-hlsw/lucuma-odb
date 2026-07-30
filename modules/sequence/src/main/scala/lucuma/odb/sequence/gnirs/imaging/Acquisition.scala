// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gnirs
package imaging

import cats.Monad
import cats.Order
import cats.data.NonEmptyList
import cats.data.State
import cats.syntax.applicative.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState.Disabled
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.imaging.ImagingSequence
import lucuma.odb.sequence.util.AtomBuilder
import lucuma.refined.*

import java.util.UUID

/**
 * GNIRS imaging acquisition sequence generation. Because the GNIRS imaging field of
 * view is small, an acquisition is used to center the target: an offset image where
 * the observer measures the "keyhole" center, followed by an image on the target.
 *
 * There are three brightness types (classified by the ITC exactly as for spectroscopy):
 *
 *   - Very Bright: offset (10,0) field image in H (Order4) with the fixed per-camera
 *     exposure, then an on-target (0,0) image in the narrow-band H2 with the ITC exposure.
 *   - Bright: offset (10,0) field image in the first science filter with the fixed
 *     per-camera exposure, then an on-target (0,0) image in the same filter with the ITC
 *     exposure.
 *   - Faint: offset (0,10) field image (for keyhole measurement and sky subtraction) and
 *     an on-target (0,0) image, both in the first science filter, both with the ITC
 *     exposure.
 *
 * As with spectroscopy, an "Initial Acquisition" atom (field image + on-target image) is
 * followed by [[RepeatingAtomCount]] "Fine Adjustments" atoms, each a single on-target image.
 */
object Acquisition:

  val RepeatingAtomCount: Int = 10

  private val SingleCoadd: PosInt = 1.refined

  case class Steps(
    fieldImage: ProtoStep[GnirsDynamicConfig],
    onTarget:   ProtoStep[GnirsDynamicConfig]
  ):
    // The on-target image carries a breakpoint so the observer can pause to apply the
    // centering measured on the field image.
    val initialAtom: NonEmptyList[ProtoStep[GnirsDynamicConfig]] =
      NonEmptyList.of(fieldImage, onTarget.withBreakpoint)

    val repeatingAtom: NonEmptyList[ProtoStep[GnirsDynamicConfig]] =
      NonEmptyList.of(onTarget)

  private object StepComputer extends GnirsSequenceState:

    def compute(
      camera:      GnirsCamera,
      acqType:     GnirsAcquisitionType,
      firstFilter: GnirsFilter,
      time:        IntegrationTime
    ): Steps =
      val acqExposureTime: TimeSpan = time.exposureTime
      // In S/N mode the ITC exposure count is the acquisition coadds.
      val acqCoadds: PosInt         = time.exposureCount
      // The fixed per-camera keyhole exposure (short 3s, long 15s).
      val camExposureTime: TimeSpan = keyholeExposureTime(camera)

      // The field/keyhole image: filter, exposure and coadds depend on the brightness type.
      // Very Bright / Bright use the fixed camera exposure and a single coadd; Faint uses the
      // ITC exposure and coadds (it doubles as a sky frame). The offset is (0,10) for Faint,
      // else (10,0).
      val (fieldFilter, fieldExposureTime, fieldCoadds, fieldOffset): (GnirsFilter, TimeSpan, PosInt, Offset) =
        acqType match
          case GnirsAcquisitionType.VeryBright =>
            (GnirsFilter.Order4, camExposureTime, SingleCoadd, Offset(Offset.P(10.arcsec), Offset.Q(0.arcsec)))
          case GnirsAcquisitionType.Bright     =>
            (firstFilter,        camExposureTime, SingleCoadd, Offset(Offset.P(10.arcsec), Offset.Q(0.arcsec)))
          case GnirsAcquisitionType.Faint      =>
            (firstFilter,        acqExposureTime, acqCoadds,   Offset(Offset.P(0.arcsec),  Offset.Q(10.arcsec)))

      // The on-target image always uses the ITC exposure and coadds. Very Bright images
      // through H2 (its exposure comes from the H2 ITC pass); the others use the first filter.
      val onTargetFilter: GnirsFilter =
        acqType match
          case GnirsAcquisitionType.VeryBright => GnirsFilter.H2
          case _                               => firstFilter

      eval:
        for
          _          <- State.modify[GnirsDynamicConfig]: dyn =>
                          dyn.copy(
                            exposure          = fieldExposureTime,
                            coadds            = fieldCoadds,
                            filter            = fieldFilter,
                            acquisitionMirror = GnirsAcquisitionMirrorMode.In,
                            camera            = camera,
                            readMode          = GnirsReadMode.forExposureTime(fieldExposureTime),
                            decker            = GnirsDecker.Acquisition,
                            fpu               = GnirsFpu.Other(GnirsFpuOther.Acquisition)
                          )
          fieldImage <- scienceStep(TelescopeConfig(fieldOffset, Disabled), ObserveClass.Acquisition)
          _          <- State.modify[GnirsDynamicConfig]:
                          _.copy(
                            exposure = acqExposureTime,
                            coadds   = acqCoadds,
                            filter   = onTargetFilter,
                            readMode = GnirsReadMode.forExposureTime(acqExposureTime)
                          )
          onTarget   <- scienceStep(0.arcsec, 0.arcsec, ObserveClass.Acquisition)
        yield Steps(fieldImage, onTarget)

  private class Generator(
    builder:       AtomBuilder[GnirsDynamicConfig],
    initialAtom:   NonEmptyList[ProtoStep[GnirsDynamicConfig]],
    repeatingAtom: NonEmptyList[ProtoStep[GnirsDynamicConfig]]
  ) extends SequenceGenerator[GnirsDynamicConfig]:

    override val generate: Stream[Pure, Atom[GnirsDynamicConfig]] =
      (for
        a0 <- builder.build(NonEmptyString.unapply("Initial Acquisition"), 0, 0, initialAtom)
        as <- (1 to RepeatingAtomCount).toList.traverse: aix =>
                builder.build(NonEmptyString.unapply("Fine Adjustments"), aix, 0, repeatingAtom)
      yield Stream.emits(a0 :: as)).runA(StepTimeEstimateCalculator.Last.empty[GnirsDynamicConfig]).value

  // `pinnedAcqType` is the acquisition mode resolved by the ITC brightness classification
  // (the two-pass acquisition ITC). When present it pins the type rather than re-deriving
  // it from the final exposure time.
  def instantiate[F[_]: Monad](
    observationId: Observation.Id,
    estimator:     StepTimeEstimateCalculator[GnirsStaticConfig, GnirsDynamicConfig],
    static:        GnirsStaticConfig,
    namespace:     UUID,
    config:        Config,
    time:          Either[OdbError, IntegrationTime],
    pinnedAcqType: Option[GnirsAcquisitionType]
  ): F[Either[OdbError, SequenceGenerator[GnirsDynamicConfig]]] =
    def sequenceError(msg: String): OdbError =
      OdbError.SequenceUnavailable(observationId, s"Could not generate a sequence for $observationId: $msg".some)

    val builder: AtomBuilder[GnirsDynamicConfig] =
      AtomBuilder.instantiate(estimator, static, namespace, SequenceType.Acquisition)

    // The first filter in the sequence: the wavelength-first filter under the variant's
    // ordering — the same ordering GeneratorParamsService used for the ITC classification.
    given Order[GnirsFilter] = ImagingSequence.wavelengthOrder(config.variant)(_.centralWavelength)
    val firstFilter: GnirsFilter = config.filters.map(_.filter).sorted.head

    (for
      t <- time.filterOrElse(
             _.exposureTime.toNonNegMicroseconds.value > 0,
             sequenceError("GNIRS Imaging requires a positive acquisition exposure time.")
           )
    yield
      val acqType: GnirsAcquisitionType =
        pinnedAcqType.getOrElse(GnirsAcquisitionMode.defaultFor(t.exposureTime, t.exposureCount).acquisitionType)
      val steps: Steps = StepComputer.compute(config.camera, acqType, firstFilter, t)
      Generator(builder, steps.initialAtom, steps.repeatingAtom): SequenceGenerator[GnirsDynamicConfig]
    ).pure[F]
