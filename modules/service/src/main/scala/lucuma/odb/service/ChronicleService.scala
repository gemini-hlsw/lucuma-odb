// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import grackle.Result
import lucuma.odb.graphql.input.ConditionsEntryInput
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.all.*
import skunk.syntax.all.*

import Services.Syntax.*

trait ChronicleService[F[_]]:
  def addConditionsEntry(input: ConditionsEntryInput)(using Services.StaffAccess): F[Result[Long]]

object ChronicleService {

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): ChronicleService[F] =
    new ChronicleService[F]:

      def addConditionsEntry(input: ConditionsEntryInput)(using Services.StaffAccess): F[Result[Long]] =
        session.prepareR(Statements.InsertConditionsEntry).use: pq =>
          pq.unique(input).map(Result.success)

  object Statements {
    
    val InsertConditionsEntry: Query[ConditionsEntryInput, Long] =
      sql"""
        INSERT INTO t_chron_conditions_entry (
          c_measurement_source,        
          c_measurement_seeing,        
          c_measurement_extinction_millimags,
          c_measurement_wavelength,    
          c_measurement_azimuth,       
          c_measurement_elevation,     
          c_intuition_expectation,     
          c_intuition_timespan,        
          c_intuition_seeing_trend    
        ) VALUES (
          ${tag.opt},
          ${angle_µas.opt},
          ${extinction.opt},
          ${wavelength_pm.opt},
          ${angle_µas.opt},
          ${angle_µas.opt},
          ${tag.opt},
          ${time_span.opt},
          ${tag.opt}
        ) RETURNING c_chron_id
      """.query(int8).contramap[ConditionsEntryInput] { cie =>

        val omeas = cie.value.left
        val ointu = cie.value.right

        omeas.map(_.source)                         *:
        omeas.flatMap(_.seeing)                     *:
        omeas.flatMap(_.extinction)                 *:
        omeas.flatMap(_.wavelength)                 *:
        omeas.flatMap(_.azimuth)                    *:
        omeas.flatMap(_.elevation)                  *:
        ointu.flatMap(_.value.left.map(_.tpe))      *:
        ointu.flatMap(_.value.left.map(_.timespan)) *:
        ointu.flatMap(_.value.right)                *:
        EmptyTuple

      }

  }

}