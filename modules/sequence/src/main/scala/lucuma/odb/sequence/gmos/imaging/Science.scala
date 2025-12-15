// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos
package imaging

import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.Observation
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.itc.IntegrationTime
import lucuma.odb.data.OdbError

import java.util.UUID


object Science:

  def gmosNorth[F[_]](
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[StaticConfig.GmosNorth, GmosNorth],
    static:        StaticConfig.GmosNorth,
    namespace:     UUID,
    config:        Config.GmosNorth,
    time:          Either[OdbError, Map[GmosNorthFilter, IntegrationTime]]
  ): F[Either[OdbError, SequenceGenerator[GmosNorth]]] =
    ???

  def gmosSouth[F[_]](
    observationId: Observation.Id,
    estimator:     TimeEstimateCalculator[StaticConfig.GmosSouth, GmosSouth],
    static:        StaticConfig.GmosSouth,
    namespace:     UUID,
    config:        Config.GmosSouth,
    time:          Either[OdbError, Map[GmosSouthFilter, IntegrationTime]]
  ): F[Either[OdbError, SequenceGenerator[GmosSouth]]] =
    ???