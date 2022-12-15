// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
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
import lucuma.odb.graphql.instances.SourceProfileCodec.given
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.circe.codec.json.*
import skunk.implicits.*

import scala.collection.immutable.SortedMap


/*
final case class SpectroscopyModeInput(
  wavelength:     Wavelength,
  signalToNoise:  PosBigDecimal,
  sourceProfile:  SourceProfile,
  band:           Band,
  radialVelocity: RadialVelocity,
  constraints:    ConstraintSet,
  mode:           InstrumentMode
)
*/

trait ItcClientService[F[_]] {

  def selectSpectroscopyInput(
    programId: Program.Id,
    which:     List[Observation.Id]
  ): F[Map[Observation.Id, NonEmptyList[SpectroscopyModeInput]]]

}

object ItcClientService {

  def fromSession[F[_]: Sync](
    session:  Session[F],
    user:     User,
    mService: ObservingModeServices[F]
  ): ItcClientService[F] =

    new ItcClientService[F] {

      private def spectroscopy(
        o: ItcParams,
        m: ObservingModeServices.ItcParams,
      ): Option[SpectroscopyModeInput] = {

        def extractBand[T](w: Wavelength, bMap: SortedMap[Band, BrightnessMeasure[T]]): Option[Band] =
          bMap.minByOption { case (b, _) =>
            (w.toPicometers.value.value - b.center.toPicometers.value.value).abs
          }.map(_._1)

        def band: Option[Band] =
          SourceProfile
            .integratedBrightnesses
            .getOption(o.sourceProfile)
            .flatMap(bMap => extractBand[Integrated](m.wavelength, bMap))
            .orElse(
              SourceProfile
                .surfaceBrightnesses
                .getOption(o.sourceProfile)
                .flatMap(bMap => extractBand[Surface](m.wavelength, bMap))
            )

        band.map { b =>
          SpectroscopyModeInput(
            m.wavelength,
            o.signalToNoise,
            o.signalToNoiseAt,
            o.sourceProfile,
            b,
            o.radialVelocity,
            o.constraints,
            m.mode
          )
        }
      }

      override def selectSpectroscopyInput(
        programId: Program.Id,
        which:     List[Observation.Id]
      ): F[Map[Observation.Id, NonEmptyList[SpectroscopyModeInput]]] =
        for {
          p <- selectItcParams(programId, which)
          m <- mService.selectItcParams(p.toList.map(_.map(_.head.observingMode)))
        } yield
          p.foldLeft(Map.empty[Observation.Id, NonEmptyList[SpectroscopyModeInput]]) { case (res, (oid, itcList)) =>
            m.get(oid).fold(res) { modeParams =>
              itcList
                .traverse { itcParams => spectroscopy(itcParams, modeParams) }
                .fold(res)(nel => res.updated(oid, nel))
            }
          }

      private def selectItcParams(
        programId: Program.Id,
        which:     List[Observation.Id]
      ): F[Map[Observation.Id, NonEmptyList[ItcParams]]] =
        NonEmptyList.fromList(which).fold(Applicative[F].pure(Map.empty[Observation.Id, NonEmptyList[ItcParams]])) { oids =>
          val (af, decoder) = Statements.selectItcParams(user, programId, oids)
          session
            .prepare(af.fragment.query(decoder))
            .use(_.stream(af.argument, chunkSize = 64).compile.to(List))
            .map(_.groupMap(_._1)(_._2).view.mapValues(ps => NonEmptyList.fromListUnsafe(ps)).toMap)
        }
    }

  final case class ItcParams(
    constraints:     ConstraintSet,
    signalToNoise:   PosBigDecimal,
    signalToNoiseAt: Option[Wavelength],
    observingMode:   ObservingModeType,
    radialVelocity:  RadialVelocity,
    sourceProfile:   SourceProfile
  )

  object Statements {

    import ProgramService.Statements.existsUserAccess

    private val source_profile: Decoder[SourceProfile] =
      jsonb.emap { sp =>
        sp.as[SourceProfile].leftMap(f => s"Could not decode SourceProfile: ${f.message}")
      }

    private val itc_params: Decoder[ItcParams] =
      (constraint_set       ~
       signal_to_noise      ~
       wavelength_pm.opt    ~
       observing_mode_type  ~
       radial_velocity      ~
       source_profile
      ).gmap[ItcParams]

    def selectItcParams(
      user:      User,
      programId: Program.Id,
      which:     NonEmptyList[Observation.Id]
    ): (AppliedFragment, Decoder[(Observation.Id, ItcParams)]) =
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
          c_sid_rv,
          c_source_profile
        FROM v_itc_params
        WHERE
          c_observation_id IN (""" |+|
            which.map(sql"$observation_id").intercalate(void", ") |+|
          void")" |+| existsUserAccess(user, programId).fold(AppliedFragment.empty) { af => void""" AND """ |+| af },
        (observation_id ~ itc_params)
      )

  }
}
