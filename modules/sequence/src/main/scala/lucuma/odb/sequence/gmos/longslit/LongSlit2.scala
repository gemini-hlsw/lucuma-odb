// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package gmos.longslit

import cats.Applicative
import cats.syntax.option.*
import fs2.Stream
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.CalibrationRole.SpectroPhotometric
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.itc.IntegrationTime
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoExecutionConfig
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StepRecord

object LongSlit2:

  case class ScienceTargetGen[F[_]: Applicative, S, D](
    expander: SmartGcalExpander[F, D]
  ) extends Generator2[F, S, D]:

    override def generate(
      acquisitionItc: IntegrationTime,
      scienceItc:     IntegrationTime,
      stepRecords:    Stream[F, StepRecord[D]]
    ): ProtoExecutionConfig[F, S, Either[String, (ProtoAtom[ProtoStep[D]], Long)]] =
      ???

  end ScienceTargetGen


  case class SpectroPhotometricGen[F[_]: Applicative, S, D](
    expander: SmartGcalExpander[F, D]
  ) extends Generator2[F, S, D]:

    override def generate(
      acquisitionItc: IntegrationTime,
      scienceItc:     IntegrationTime,
      stepRecords:    Stream[F, StepRecord[D]]
    ): ProtoExecutionConfig[F, S, Either[String, (ProtoAtom[ProtoStep[D]], Long)]] =
      ???

  end SpectroPhotometricGen


  private def instantiate[F[_]: Applicative, S, D](
    expander: SmartGcalExpander[F, D],
    calRole:  Option[CalibrationRole]
  ): Option[Generator2[F, S, D]] =

    calRole match
      case None                     => ScienceTargetGen(expander).some
      case Some(SpectroPhotometric) => SpectroPhotometricGen(expander).some
      case _                        => none

  def gmosNorth[F[_]: Applicative](
    expander: SmartGcalExpander[F, DynamicConfig.GmosNorth],
    calRole:  Option[CalibrationRole]
  ): Option[Generator2[F, StaticConfig.GmosNorth, DynamicConfig.GmosNorth]] =
    instantiate(expander, calRole)

  def gmosSouth[F[_]: Applicative](
    expander: SmartGcalExpander[F, DynamicConfig.GmosSouth],
    calRole:  Option[CalibrationRole]
  ): Option[Generator2[F, StaticConfig.GmosSouth, DynamicConfig.GmosSouth]] =
    instantiate(expander, calRole)

end LongSlit2