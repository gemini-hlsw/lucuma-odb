// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.data.ValidatedNel
import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.syntax.validated.*
import edu.gemini.grackle.Result
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.BrightnessMeasure
import lucuma.core.math.BrightnessUnits.Integrated
import lucuma.core.math.BrightnessUnits.Surface
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.SpectroscopyModeInput
import lucuma.odb.data.ObservingModeType
import lucuma.odb.json.angle.decoder.given
import lucuma.odb.json.sourceprofile.given
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.circe.codec.json.*
import skunk.implicits.*

import scala.collection.immutable.SortedMap

/**
 * A database query service to gather input parameters to send to the ITC.
 */
trait ItcInputService[F[_]] {

  def selectSpectroscopyInput(
    programId: Program.Id,
    which:     List[Observation.Id]
  ): F[Map[Observation.Id, EitherNel[(Option[Target.Id], String), NonEmptyList[(Target.Id, SpectroscopyModeInput)]]]]

}

object ItcInputService {

  def fromSession[F[_]: Sync](
    session:  Session[F],
    user:     User,
    mService: ObservingModeServices[F]
  ): ItcInputService[F] =

    new ItcInputService[F] {

      private def spectroscopy(
        o: ItcParams,
        m: ObservingModeServices.ItcParams,
      ): ValidatedNel[(Option[Target.Id], String), (Target.Id, SpectroscopyModeInput)] = {

        def extractBand[T](w: Wavelength, bMap: SortedMap[Band, BrightnessMeasure[T]]): Option[Band] =
          bMap.minByOption { case (b, _) =>
            (w.toPicometers.value.value - b.center.toPicometers.value.value).abs
          }.map(_._1)

        def band: Option[Band] =
          o.sourceProfile.flatMap { sp =>
            SourceProfile
              .integratedBrightnesses
              .getOption(sp)
              .flatMap(bMap => extractBand[Integrated](m.wavelength, bMap))
              .orElse(
                SourceProfile
                  .surfaceBrightnesses
                  .getOption(sp)
                  .flatMap(bMap => extractBand[Surface](m.wavelength, bMap))
              )
          }

        (o.signalToNoise.toValidNel(none[Target.Id] -> "signal to noise"),
         o.targetId.toValidNel(none[Target.Id] -> "target")
        ).tupled.andThen { case (s2n, tid) =>
          // these are dependent on having a target in the first place
          (band.toValidNel(tid.some -> "brightness measure"),
           o.radialVelocity.toValidNel(tid.some -> "radial velocity"),
           o.sourceProfile.toValidNel(tid.some -> "source profile")
          ).mapN { case (b, rv, sp) =>
            tid ->
            SpectroscopyModeInput(
              m.wavelength,
              s2n,
              o.signalToNoiseAt,
              sp,
              b,
              rv,
              o.constraints,
              m.mode
            )
          }
        }
      }

      override def selectSpectroscopyInput(
        programId: Program.Id,
        which:     List[Observation.Id]
      ): F[Map[Observation.Id, EitherNel[(Option[Target.Id], String), NonEmptyList[(Target.Id, SpectroscopyModeInput)]]]] =
        for {
          p  <- selectItcParams(programId, which)
          oms = p.collect { case ItcParams(oid, _, _, _, Some(om), _, _, _) => (oid, om) }.distinct
          m  <- mService.selectItcParams(oms)
        } yield
          p.map { itcParams =>
            val oid = itcParams.observationId
            (oid,
             m.get(oid).toValidNel(none[Target.Id] -> "observing mode").andThen { om =>
               spectroscopy(itcParams, om)
             }
            )
          }.groupMap(_._1)(_._2)
           .view
           .mapValues(_.sequence.toEither.map(NonEmptyList.fromListUnsafe))
           .toMap

      private def selectItcParams(
        programId: Program.Id,
        which:     List[Observation.Id]
      ): F[List[ItcParams]] =
        NonEmptyList
          .fromList(which)
          .fold(List.empty[ItcParams].pure[F]) { oids =>
            val (af, decoder) = Statements.selectItcParams(user, programId, oids)
            session
              .prepare(af.fragment.query(decoder))
              .use(_.stream(af.argument, chunkSize = 64).compile.to(List))
          }
    }

  final case class ItcParams(
    observationId:   Observation.Id,
    constraints:     ConstraintSet,
    signalToNoise:   Option[PosBigDecimal],
    signalToNoiseAt: Option[Wavelength],
    observingMode:   Option[ObservingModeType],
    targetId:        Option[Target.Id],
    radialVelocity:  Option[RadialVelocity],
    sourceProfile:   Option[SourceProfile]
  )

  object Statements {

    import ProgramService.Statements.existsUserAccess

    private val source_profile: Decoder[SourceProfile] =
      jsonb.emap { sp =>
        sp.as[SourceProfile].leftMap(f => s"Could not decode SourceProfile: ${f.message}")
      }

    private val itc_params: Decoder[ItcParams] =
      (observation_id          ~
       constraint_set          ~
       signal_to_noise.opt     ~
       wavelength_pm.opt       ~
       observing_mode_type.opt ~
       target_id.opt           ~
       radial_velocity.opt     ~
       source_profile.opt
      ).gmap[ItcParams]

    def selectItcParams(
      user:      User,
      programId: Program.Id,
      which:     NonEmptyList[Observation.Id]
    ): (AppliedFragment, Decoder[ItcParams]) =
      (void"""
        SELECT
          c_observation_id,
          c_image_quality,
          c_cloud_extinction,
          c_sky_background,
          c_water_vapor,
          c_air_mass_min,
          c_air_mass_max,
          c_hour_angle_min,
          c_hour_angle_max,
          c_spec_signal_to_noise,
          c_spec_signal_to_noise_at,
          c_observing_mode_type,
          c_target_id,
          c_sid_rv,
          c_source_profile
        FROM v_itc_params
        WHERE
          c_observation_id IN (""" |+|
            which.map(sql"$observation_id").intercalate(void", ") |+|
          void")" |+| existsUserAccess(user, programId).fold(AppliedFragment.empty) { af => void""" AND """ |+| af },
        itc_params
      )

  }
}
