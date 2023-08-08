// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Eq
import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.data.ValidatedNel
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.BrightnessMeasure
import lucuma.core.math.BrightnessUnits.Integrated
import lucuma.core.math.BrightnessUnits.Surface
import lucuma.core.math.RadialVelocity
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.itc.client.GmosFpu
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.SpectroscopyIntegrationTimeInput
import lucuma.odb.data.ObservingModeType
import lucuma.odb.json.sourceprofile.given
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.circe.codec.json.*
import skunk.implicits.*

import scala.collection.immutable.SortedMap

import GeneratorParamsService.Error
import Services.Syntax.*

trait GeneratorParamsService[F[_]] {

  def selectOne(
    programId:     Program.Id,
    observationId: Observation.Id
  )(using Transaction[F]): F[EitherNel[Error, GeneratorParams]]

  def selectMany(
    programId:      Program.Id,
    observationIds: List[Observation.Id]
  )(using Transaction[F]): F[Map[Observation.Id, EitherNel[Error, GeneratorParams]]]

  def selectAll(
    programId: Program.Id,
  )(using Transaction[F]): F[Map[Observation.Id, EitherNel[Error, GeneratorParams]]]

}

object GeneratorParamsService {

  sealed trait Error extends Product with Serializable {
    def format: String
  }

  object Error {
    case class MissingObservation(programId: Program.Id, observationId: Observation.Id) extends Error {
      def format: String =
        s"Observation '$observationId' in program '$programId' not found."
    }

    case class MissingData(targetId: Option[Target.Id], paramName: String) extends Error {
      def format: String =
        s"${targetId.map(tid => s"(target $tid) ").orEmpty}$paramName"
    }

    case object ConflictingData extends Error {
      def format: String =
        "Conflicting data, all stars in the asterism must use the same observing mode and parameters."
    }

    def missing(paramName: String): Error =
      MissingData(none, paramName)

    def targetMissing(tid: Target.Id, paramName: String): Error =
      MissingData(tid.some, paramName)

    given Eq[Error] with {
      def eqv(x: Error, y: Error): Boolean =
        (x, y) match {
          case (MissingData(t0, p0), MissingData(t1, p1)) => (t0 === t1) && (p0 === p1)
          case (ConflictingData, ConflictingData)         => true
          case _                                          => false
        }
    }

  }

  def instantiate[F[_]: Concurrent](using Services[F]): GeneratorParamsService[F] =
    new GeneratorParamsService[F] {

      import lucuma.odb.sequence.gmos

      override def selectOne(
        pid: Program.Id,
        oid: Observation.Id
      )(using Transaction[F]): F[EitherNel[Error, GeneratorParams]] =
        selectMany(pid, List(oid)).map(_.getOrElse(oid, Error.MissingObservation(pid, oid).leftNel))

      override def selectMany(
        pid:  Program.Id,
        oids: List[Observation.Id]
      )(using Transaction[F]): F[Map[Observation.Id, EitherNel[Error, GeneratorParams]]] =
        doSelect(selectManyParams(pid, oids))

      override def selectAll(
        pid: Program.Id,
      )(using Transaction[F]): F[Map[Observation.Id, EitherNel[Error, GeneratorParams]]] =
        doSelect(selectAllParams(pid))

      private def doSelect(
        params: F[List[Params]]
      )(using Transaction[F]): F[Map[Observation.Id, EitherNel[Error, GeneratorParams]]] =
        for {
          ps <- params
          oms = ps.collect { case Params(oid, _, _, _, Some(om), _, _, _) => (oid, om) }.distinct
          m  <- observingModeServices.selectObservingMode(oms)
        } yield
          ps.groupBy(_.observationId).map { case (oid, oParams) =>
            oid -> toGeneratorParams(oParams, m.get(oid))
          }

      private def selectManyParams(
        pid:  Program.Id,
        oids: List[Observation.Id]
      ): F[List[Params]] =
        NonEmptyList
          .fromList(oids)
          .fold(List.empty[Params].pure[F]) { oids =>
            executeSelect(Statements.selectManyParams(user, pid, oids))
          }

      private def selectAllParams(
        pid: Program.Id
      ): F[List[Params]] =
        executeSelect(Statements.selectAllParams(user, pid))

      private def executeSelect(af: AppliedFragment): F[List[Params]] =
        session
          .prepareR(af.fragment.query(Statements.params))
          .use(_.stream(af.argument, chunkSize = 64).compile.to(List))

      private def observingMode(
        params: List[Params],
        config: Option[SourceProfile => ObservingMode]
      ): EitherNel[Error, ObservingMode] = {
        val configs: EitherNel[Error, List[ObservingMode]] =
          params.traverse { p =>
            for {
              t <- p.targetId.toRightNel(Error.missing("target"))
              s <- p.sourceProfile.toRightNel(Error.MissingData(t.some, "source profile"))
              f <- config.toRightNel(Error.missing("observing mode"))
            } yield f(s)
          }

        // All of the `ObservingMode`s that we compute have to be the same.
        // Otherwise we would need to configure the instrument differently for
        // different stars in the asterism.
        configs.flatMap { scs =>
          scs.distinct match {
            case sc :: Nil => sc.rightNel
            case _         => Error.ConflictingData.leftNel
          }
        }
      }

      private def toGeneratorParams(
        params: List[Params],
        config: Option[SourceProfile => ObservingMode]
      ): EitherNel[Error, GeneratorParams] =
        observingMode(params, config).flatMap {
          case gn @ gmos.longslit.Config.GmosNorth(g, f, u, λ, _, _, _, _, _, _, _, _) =>
            params.traverse { p =>
              spectroscopyParams(
                p,
                InstrumentMode.GmosNorthSpectroscopy(
                  g,
                  f,
                  GmosFpu.North.builtin(u),
                  gn.ccdMode.some,
                  gn.roi.some),
                λ)
            }.map { itcParams =>
              GeneratorParams.GmosNorthLongSlit(NonEmptyList.fromListUnsafe(itcParams), gn)
            }.toEither

          case gs @ gmos.longslit.Config.GmosSouth(g, f, u, λ, _, _, _, _, _, _, _, _) =>
            params.traverse { p =>
              spectroscopyParams(
                p,
                InstrumentMode.GmosSouthSpectroscopy(
                  g,
                  f,
                  GmosFpu.South.builtin(u),
                  gs.ccdMode.some,
                  gs.roi.some),
                λ)
            }.map { itcParams =>
              GeneratorParams.GmosSouthLongSlit(NonEmptyList.fromListUnsafe(itcParams), gs)
            }.toEither
        }

      private def spectroscopyParams(
        params:     Params,
        mode:       InstrumentMode,
        wavelength: Wavelength
      ): ValidatedNel[Error, (Target.Id, SpectroscopyIntegrationTimeInput)] = {

        def extractBand[T](w: Wavelength, bMap: SortedMap[Band, BrightnessMeasure[T]]): Option[Band] =
          bMap.minByOption { case (b, _) =>
            (w.toPicometers.value.value - b.center.toPicometers.value.value).abs
          }.map(_._1)

        def band: Option[Band] =
          params.sourceProfile.flatMap { sp =>
            SourceProfile
              .integratedBrightnesses
              .getOption(sp)
              .flatMap(bMap => extractBand[Integrated](wavelength, bMap))
              .orElse(
                SourceProfile
                  .surfaceBrightnesses
                  .getOption(sp)
                  .flatMap(bMap => extractBand[Surface](wavelength, bMap))
              )
          }

        (params.signalToNoise.toValidNel(Error.missing("signal to noise")),
         params.signalToNoiseAt.toValidNel(Error.missing("signal to noise at wavelength")),
         params.targetId.toValidNel(Error.missing("target"))
        ).tupled.andThen { case (s2n, s2nA, tid) =>
          // these are dependent on having a target in the first place
          (band.toValidNel(Error.targetMissing(tid, "brightness measure")),
           params.radialVelocity.toValidNel(Error.targetMissing(tid, "radial velocity")),
           params.sourceProfile.toValidNel(Error.targetMissing(tid, "source profile"))
          ).mapN { case (b, rv, sp) =>
            tid ->
            SpectroscopyIntegrationTimeInput(
              wavelength,
              s2n,
              s2nA.some,
              sp,
              b,
              rv,
              params.constraints,
              mode
            )
          }
        }

      }

    }


  final case class Params(
    observationId:   Observation.Id,
    constraints:     ConstraintSet,
    signalToNoise:   Option[SignalToNoise],
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

    val params: Decoder[Params] =
      (observation_id          *:
       constraint_set          *:
       signal_to_noise.opt     *:
       wavelength_pm.opt       *:
       observing_mode_type.opt *:
       target_id.opt           *:
       radial_velocity.opt     *:
       source_profile.opt
      ).to[Params]

    private def ParamColumns(tab: String): String =
      s"""
        $tab.c_observation_id,
        $tab.c_image_quality,
        $tab.c_cloud_extinction,
        $tab.c_sky_background,
        $tab.c_water_vapor,
        $tab.c_air_mass_min,
        $tab.c_air_mass_max,
        $tab.c_hour_angle_min,
        $tab.c_hour_angle_max,
        $tab.c_spec_signal_to_noise,
        $tab.c_spec_signal_to_noise_at,
        $tab.c_observing_mode_type,
        $tab.c_target_id,
        $tab.c_sid_rv,
        $tab.c_source_profile
      """

    def selectManyParams(
      user:      User,
      programId: Program.Id,
      which:     NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT
          #${ParamColumns("gp")}
        FROM v_generator_params gp
        WHERE
      """(Void) |+|
        sql"""gp.c_program_id = $program_id""".apply(programId) |+|
        void""" AND gp.c_observation_id IN (""" |+|
          which.map(sql"$observation_id").intercalate(void", ") |+|
        void")" |+|
        existsUserAccess(user, programId).fold(AppliedFragment.empty) { af => void""" AND """ |+| af }

    def selectAllParams(
      user:      User,
      programId: Program.Id
    ): AppliedFragment =
      sql"""
        SELECT
          #${ParamColumns("gp")}
        FROM v_generator_params gp
        INNER JOIN t_observation ob ON gp.c_observation_id = ob.c_observation_id
        WHERE
      """(Void) |+|
        sql"""gp.c_program_id = $program_id""".apply(programId) |+|
        void""" AND ob.c_existence = 'present' """ |+|
        existsUserAccess(user, programId).fold(AppliedFragment.empty) { af => void""" AND """ |+| af }
  }
}
