// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import edu.gemini.grackle.Result
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.{BrightnessMeasure, Integrated, Surface}
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.SpectroscopyModeInput

import scala.collection.immutable.SortedMap
import skunk.Session


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
    oService: ObservationService[F],
    mService: ObservingModeServices[F],
    aService: AsterismService[F],
    tService: TargetService[F]
  ): ItcClientService[F] =

    new ItcClientService[F] {

      private def spectroscopy(
        o: ObservationService.ItcParams,
        m: ObservingModeServices.ItcParams,
        t: TargetService.ItcParams
      ): Option[SpectroscopyModeInput] = {

        def extractBand[T](w: Wavelength, bMap: SortedMap[Band, BrightnessMeasure[T]]): Option[Band] =
          bMap.minByOption { case (b, _) =>
            (w.toPicometers.value.value - b.center.toPicometers.value.value).abs
          }.map(_._1)

        def band: Option[Band] =
          SourceProfile
            .integratedBrightnesses
            .getOption(t.sourceProfile)
            .flatMap(bMap => extractBand[Integrated](m.wavelength, bMap))
            .orElse(
              SourceProfile
                .surfaceBrightnesses
                .getOption(t.sourceProfile)
                .flatMap(bMap => extractBand[Surface](m.wavelength, bMap))
            )

        band.map { b =>
          SpectroscopyModeInput(
            m.wavelength,
            o.signalToNoise,
            o.signalToNoiseAt,
            t.sourceProfile,
            b,
            t.radialVelocity,
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
          // Select ITC data from the observation table
          o <- oService.selectItcParams(programId, which)

          // Using the observing modes in the return value above, select the ITC
          // observing mode data
          m <- mService.selectItcParams(o.toList.map { case (oid, itc) => (oid, itc.observingMode) })

          // Obtain the asterisms for each of the observations.
          a <- NonEmptyList.fromList(which).fold(Map.empty[Observation.Id, List[Target.Id]].pure[F]) { oids =>
            aService.selectAsterism(programId, oids)
          }

          // ITC params for each of the targets in the collection of asterisms
          t <- tService.selectItcParams(a.values.toList.flatten.distinct)
        } yield

          // Combine all the ITC parameter data into SpectroscopyModeInput when
          // possible.
          o.keySet
           .intersect(m.keySet)
           .intersect(a.keySet)
           .foldLeft(Map.empty[Observation.Id, NonEmptyList[SpectroscopyModeInput]]) { (r, oid) =>
             NonEmptyList.fromList(
               a(oid)
                 .flatMap(t.get(_).toList)
                 .flatMap(spectroscopy(o(oid), m(oid), _).toList)
             ).fold(r) { nel => r.updated(oid, nel) }
           }

    }
}
