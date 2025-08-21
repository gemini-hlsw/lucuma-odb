// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import io.circe.Json
import lucuma.core.model.Program
import lucuma.core.model.sequence.Dataset
import lucuma.odb.util.Codecs.*
import skunk.circe.codec.json.jsonb
import skunk.syntax.all.*

trait ChronicleOperations { this: OdbSuite =>

  def chronAsterismUpdates(pid: Program.Id): IO[List[Json]] =
    withSession { s =>
      s.prepareR(
        sql"""
          SELECT to_jsonb(row_to_json(t)) - ARRAY['c_timestamp', 'c_transaction_id', 'c_chron_id']
          FROM t_chron_asterism_target_update t
          WHERE t.c_program_id = $program_id
          ORDER BY c_chron_id ASC
        """.query(jsonb)
      ).use(_.stream(pid, 1024).compile.toList)
    }

  def chronDatasetUpdates(did: Dataset.Id): IO[List[Json]] =
    withSession: s =>
      s.prepareR(
        sql"""
          SELECT to_jsonb(row_to_json(t)) - ARRAY['c_timestamp', 'c_transaction_id', 'c_chron_id']
          FROM t_chron_dataset_update t
          WHERE t.c_dataset_id = $dataset_id
          ORDER BY c_chron_id ASC
        """.query(jsonb)
      ).use(_.stream(did, 1024).compile.toList)

  def chronProgramUpdates(pid: Program.Id): IO[List[Json]] =
    withSession { s =>
      s.prepareR(
        sql"""
          SELECT to_jsonb(row_to_json(t)) - ARRAY['c_timestamp', 'c_transaction_id', 'c_chron_id'] 
          FROM t_chron_program_update t 
          WHERE t.c_program_id = $program_id 
          ORDER BY c_chron_id ASC
        """.query(jsonb)
      ).use(_.stream(pid, 1024).compile.toList)
    }

}

