// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import cats.effect.kernel.Resource
import grackle.Result
import grackle.skunk.SkunkMapping
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.odb.data.Cone
import lucuma.odb.service.Services

/** The candidate lookups behind the `targetCoordinates` cone WHERE rewrite,
 *  one per cone entity.  `ConeFilter.resolve` swaps the ids into the compiled
 *  query in place of the cone placeholders.
 */
trait ConeCandidatesMapping[F[_]] { this: SkunkMapping[F] =>

  def services: Resource[F, Services[F]]

  /** Selects the ids of configuration requests whose target reference
   *  coordinates lie within `cone` (exact great-circle). */
  def configurationRequestConeCandidates(cone: Cone): F[Result[List[ConfigurationRequest.Id]]] =
    services.use(_.configurationService.coneCandidates(cone))

  /** Selects the ids of observations whose stored J2000 base position lies
   *  within `cone` (exact great-circle). */
  def observationConeCandidates(cone: Cone): F[Result[List[Observation.Id]]] =
    services.use(_.observationService.coneCandidates(cone))
}
