// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.numeric.int8
import skunk.codec.text.text

trait SummaryFailureView[F[_]] extends BaseMapping[F]:

  // Backed by v_summary_failure (V1314): the failed jobs of a program with
  // nothing left in flight.
  object SummaryFailureView extends TableDef("v_summary_failure"):
    val Id        = col("c_summary_job_id", int8)
    val ProgramId = col("c_program_id", program_id)
    val Partner   = col("c_partner", partner.opt)
    val Message   = col("c_error", text)
