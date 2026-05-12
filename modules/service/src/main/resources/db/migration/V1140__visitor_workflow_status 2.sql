-- Update `t_observation` to store `c_declared_state` as an execution state rather than
-- the boolean `c_declared_complete`. This is a multi-step process that requires re-creating
-- two views and multiple migration files because commits are necessary.

-- Continued from previous migration

-- Add `c_declared_state`.
ALTER TABLE t_observation
  ADD c_declared_state e_execution_state NULL
  CHECK (
    c_declared_state is null or
    c_declared_state = 'declared_complete'::e_execution_state or
    c_declared_state = 'declared_ongoing'::e_execution_state
  );

-- Initialize the `c_declared_state` from `c_declared_complete`.
UPDATE t_observation
  SET c_declared_state = 'declared_complete'::e_execution_state
  WHERE c_declared_complete = true;

-- Drop views that depend on `c_declared_complete`
DROP VIEW v_observation;
DROP VIEW v_generator_params;

-- Continued in next migration