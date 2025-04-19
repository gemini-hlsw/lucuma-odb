// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.foldable.*
import cats.syntax.functor.*
import lucuma.core.enums.Band
import lucuma.core.model.Observation
import lucuma.core.model.SourceProfile
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.data.Completion
import skunk.implicits.*

extension [F[_]: MonadCancelThrow](s: Session[F])
  def executeCommand(af: AppliedFragment): F[Completion] =
    s.prepareR(af.fragment.command).use(_.execute(af.argument))

  def exec(af: AppliedFragment): F[Unit] = executeCommand(af).void

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

def observationIdIn(
  oids: NonEmptyList[Observation.Id]
): AppliedFragment =
  void"c_observation_id IN (" |+|
    oids.map(sql"$observation_id").intercalate(void", ") |+|
  void")"
