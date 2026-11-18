// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server

import cats.effect.Concurrent
import lucuma.odb.StartupChecks.*
import resource.server.Codecs.*
import skunk.Session

object StartupDiagnostics:

  def checks[F[_]: Concurrent](using Session[F]): List[Check[F]] =
    List(
      checkPostgresEnum(component_location),
      checkPostgresEnum(existence),
      checkPostgresEnum(instrument_component_type),
      checkPostgresEnum(instrument_place),
      checkPostgresEnum(moon_phase),
      checkPostgresEnum(partner),
      checkPostgresEnum(power_source),
      checkPostgresEnum(resource_instrument),
      checkPostgresEnum(resource_usage),
      checkPostgresEnum(site),
      checkPostgresEnum(telescope_availability),
      checkPostgresEnum(telescope_mode_type),
      checkPostgresEnum(telescope_subsystem),
      checkPostgresEnum(too_support),
      checkEnumCoverage
    )
