
-- Update `t_observation` to store `c_declared_state` as an execution state rather than
-- the boolean `c_declared_complete`, but compute it to limit the extent of the change.
-- This is a multi-step process that requires re-creating two views.

-- Add the new enum value.
ALTER TYPE e_execution_state ADD VALUE 'declared_ongoing';

-- Continued in next migration