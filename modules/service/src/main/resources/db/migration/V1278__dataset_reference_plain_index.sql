-- Drops the redundant UNIQUE on t_dataset.c_dataset_reference.  Same pathology
-- as V1270, same fix, different table -- and this one deadlocks rather than just
-- blocking.
--
-- WHAT HAPPENED
--
-- Observe sends a step's ODB events concurrently.  Two dataset events for the
-- same dataset landing together made the ODB return HTTP 500; the cause is
-- Postgres 40P01, deadlock detected, on the event insert.  Measured against the
-- dev ODB: two in flight fail ~1 in 6, five in flight ~2 in 5.
--
-- c_dataset_reference is GENERATED and UNIQUE (V0851).  A generated column is
-- recomputed and counted as touched on every update, and because it sits in a
-- unique index that makes EVERY update of a dataset row a key update, which takes
-- FOR UPDATE.  FOR UPDATE is the only mode that conflicts with FOR KEY SHARE --
-- which is exactly what inserting an execution event takes on the dataset row it
-- references.  So:
--
--   A: addDatasetEvent(StartExpose).  INSERT takes KEY SHARE on the dataset row
--      (FK c_dataset_id), the AFTER triggers take the t_observation_execution
--      mutex, then insertDatasetEvent's setDatasetTime runs
--          UPDATE t_dataset SET c_start_time = ...
--      which wants FOR UPDATE on the dataset row and blocks on B's KEY SHARE.
--
--   B: any other addDatasetEvent for the same dataset.  Its INSERT already holds
--      KEY SHARE on the same row and its AFTER trigger blocks on A's mutex.
--
-- A waits for B's row lock, B waits for A's mutex.  Deadlock.  Note this is the
-- mirror image of the class V1227 removed: there the strong lock was taken on a
-- row the FK machinery KEY SHAREs, here the strong lock is forced by an index.
-- Only the dataset path is affected, because only it updates a row that the same
-- transaction's event insert has already KEY SHAREd -- which is why concurrent
-- step events are fine (18/18 clean at 6-way concurrency) and dataset events are
-- not.
--
-- WHAT CHANGES
--
-- Drop the UNIQUE.  i_dataset_reference (V0851, non-unique) already exists and is
-- what the lookup in DatasetService (`WHERE c_dataset_reference = $1`) uses, so
-- no index is added or removed here.  Nothing does ON CONFLICT on this column.
--
-- Uniqueness still holds by construction.  The reference is
-- format_dataset_reference(c_observation_reference, c_step_index, c_exposure_index)
-- and each part is already pinned:
--
--   * observation references are unique (V1270);
--   * c_step_index is the step's execution order, unique per observation via
--     t_step_execution_c_observation_id_c_execution_order_key and assigned under
--     the t_observation_execution mutex (V1227);
--   * t_dataset_c_step_id_c_exposure_index_key makes c_exposure_index unique per
--     step.
--
-- RESULT
--
-- Dataset updates take FOR NO KEY UPDATE and no longer conflict with the FK lock
-- the event insert holds.  Verified on a local copy of this schema, replaying the
-- exact two-transaction workload above at 8 clients: 72% of transactions failed
-- with 40P01 before, 0% after, and throughput went from 0.39 to 873 tps.

ALTER TABLE t_dataset
  DROP CONSTRAINT t_dataset_c_dataset_reference_key;

COMMENT ON COLUMN t_dataset.c_dataset_reference IS
  'Generated from the observation reference, step index and exposure index. '
  'Unique by construction rather than by constraint (see V1273): a generated '
  'column in a unique index forces every update of the row to take FOR UPDATE, '
  'which deadlocks against the FK lock held by concurrent execution event '
  'inserts.';
