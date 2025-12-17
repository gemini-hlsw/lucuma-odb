// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.IO
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.sequence.gmos.imaging.Filter


class GmosImagingServiceSuite extends ExecutionTestSupportForGmos:

  def filter(f: GmosNorthFilter): Filter[GmosNorthFilter] =
    val sn = SignalToNoise.fromInt(100).get
    val at = Wavelength.fromIntPicometers(1_210_000).get
    Filter(f, ExposureTimeMode.SignalToNoiseMode(sn, at))

  val expected1: NonEmptyList[Filter[GmosNorthFilter]] =
    NonEmptyList.of(filter(GmosNorthFilter.GPrime), filter(GmosNorthFilter.RPrime))

  val setup: IO[(Program.Id, Target.Id)] =
    for
      p  <- createProgram
      t  <- createTargetWithProfileAs(pi, p)
    yield (p, t)

  test("select filters with exposure time mode"):

    val filters = setup.flatMap: (p, t) =>
      createGmosNorthImagingObservationAs(pi, p, t)
        .parReplicateA(2)
        .flatMap: os =>
          withServices(staff): services =>
            services.gmosImagingService.selectNorth(os).map: m =>
              m.values
               .toList
               .map(_.filters)

    assertIO(filters, List(expected1, expected1))

  test("select empty"):
    val filters = setup.flatMap: (p, t) =>
      createGmosSouthImagingObservationAs(pi, p, t)  // <- south
        .parReplicateA(2)
        .flatMap: os =>
          withServices(staff): services =>
            services.gmosImagingService.selectNorth(os).map: m =>  // <- north
              m.values
               .toList
               .map(_.filters)

    assertIO(filters, List.empty)
