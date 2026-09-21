// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.requests

import cats.data.NonEmptyChain
import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.SourceProfile
import lucuma.itc.input.TargetDataInput
import lucuma.itc.legacy.LegacyBands.legacyBandsOnly
import lucuma.itc.service.TargetData

trait ServiceRequest:
  val target: TargetData

private def isEmissionLines(sourceProfile: SourceProfile): Boolean =
  SourceProfile.integratedEmissionLinesSpectralDefinition.getOption(sourceProfile).isDefined ||
    SourceProfile.surfaceEmissionLinesSpectralDefinition.getOption(sourceProfile).isDefined

private def hasBrightnesses(sourceProfile: SourceProfile): Boolean =
  SourceProfile.integratedBrightnesses
    .getOption(sourceProfile)
    .orElse(SourceProfile.surfaceBrightnesses.getOption(sourceProfile))
    .exists(_.nonEmpty)

extension (asterism: List[TargetDataInput])
  def targetInputsToData: Result[NonEmptyChain[TargetData]] =
    for {
      t <- Result.fromOption(NonEmptyChain.fromSeq(asterism), "No targets provided")
      r <- t.traverse: targetDataInput =>
             // Bands the legacy ITC cannot name must not be candidates for `nearestBand`.
             val sourceProfile = targetDataInput.sourceProfile.legacyBandsOnly
             for
               z <- Result.fromOption(
                      targetDataInput.radialVelocity.toRedshift,
                      s"Invalid radial velocity: ${targetDataInput.radialVelocity}"
                    )
               _ <- if (isEmissionLines(sourceProfile)) Result.unit
                    else
                      Result.fromOption(
                        SourceProfile.unnormalizedSED.getOption(sourceProfile).flatten,
                        "No SED provided. a SED is required for all targets"
                      ) *>
                        (if (hasBrightnesses(sourceProfile)) Result.unit
                         else if (hasBrightnesses(targetDataInput.sourceProfile))
                           Result.failure(
                             "The ITC does not support GAIA bands. At least one non-GAIA brightness measure is required."
                           )
                         else Result.failure("No brightness measures provided for target."))
             yield TargetData(sourceProfile, z)
    } yield r
