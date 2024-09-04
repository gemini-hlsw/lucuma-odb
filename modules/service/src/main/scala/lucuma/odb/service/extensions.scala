// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import lucuma.core.enums.Band
import lucuma.core.model.SourceProfile
import skunk.*
import skunk.data.Completion

extension [F[_]: MonadCancelThrow](s: Session[F]) def executeCommand(af: AppliedFragment): F[Completion] =
  s.prepareR(af.fragment.command).use(_.execute(af.argument))

private val gaiaBands: Set[Band] =
  Set(Band.Gaia, Band.GaiaBP, Band.GaiaRP)

extension (self: SourceProfile)
  // Remove GAIA bands until the ITC supports them.
  def gaiaFree: SourceProfile =
    SourceProfile
      .integratedBrightnesses
      .modifyOption(_.removedAll(gaiaBands))(self)
      .orElse(
        SourceProfile
          .surfaceBrightnesses
          .modifyOption(_.removedAll(gaiaBands))(self)
      )
      .getOrElse(self)