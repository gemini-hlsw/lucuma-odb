// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.requests

import cats.data.NonEmptyChain
import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.SourceProfile
import lucuma.itc.input.TargetDataInput
import lucuma.itc.service.TargetData

trait ServiceRequest:
  val target: TargetData

extension (asterism: List[TargetDataInput])
  def targetInputsToData: Result[NonEmptyChain[TargetData]] =
    for {
      t <- Result.fromOption(NonEmptyChain.fromSeq(asterism), "No targets provided")
      r <- t.traverse: targetDataInput =>
             for
               z <- Result.fromOption(
                      targetDataInput.radialVelocity.toRedshift,
                      s"Invalid radial velocity: ${targetDataInput.radialVelocity}"
                    )
               _ <- {
                 val isEmmisionLines = SourceProfile.integratedEmissionLinesSpectralDefinition
                   .getOption(targetDataInput.sourceProfile)
                   .isDefined ||
                   SourceProfile.surfaceEmissionLinesSpectralDefinition
                     .getOption(targetDataInput.sourceProfile)
                     .isDefined
                 if (isEmmisionLines) Result.unit
                 else
                   Result.fromOption(
                     SourceProfile.unnormalizedSED.getOption(targetDataInput.sourceProfile).flatten,
                     "No SED provided. a SED is required for all targets"
                   )
               }
             yield TargetData(targetDataInput.sourceProfile, z)
    } yield r
