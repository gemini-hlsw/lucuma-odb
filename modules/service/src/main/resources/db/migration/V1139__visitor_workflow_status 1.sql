-- Update `t_observation` to store `c_declared_state` as an execution state rather than
-- the boolean `c_declared_complete`. This is a multi-step process that requires re-creating
-- two views and multiple migration files because commits are necessary.

-- Add the new enum value.
ALTER TYPE e_execution_state ADD VALUE 'declared_ongoing';

-- Continued in next migration