// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.odb.util.Codecs.step_id
import lucuma.odb.util.Igrins2Codecs.igrins_2_dynamic
import skunk.*
import skunk.implicits.*

object Igrins2SequenceService:

  object Statements:

    val Igrins2DynamicColumns: List[String] =
      List("c_exposure_time")

    val InsertDynamic: Command[(Step.Id, Igrins2DynamicConfig)] =
      sql"""
        INSERT INTO t_igrins_2_dynamic (
          c_step_id,
          #${encodeColumns(none, Igrins2DynamicColumns)}
        ) SELECT
          $step_id,
          $igrins_2_dynamic
      """.command
