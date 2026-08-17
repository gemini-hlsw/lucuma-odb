-- Part 1 of 2.  Adds the fourth and topmost rung to the observation's execution
-- requirement ladder, ahead of renaming the whole thing to the scheduling mode
-- in V1260.
--
-- 'interrupting' means this observation may interrupt one that is already
-- executing.  It sits above 'uninterruptible' precisely because an observation
-- aggressive enough to interrupt others must never itself be interrupted: an
-- interrupting Target of Opportunity can never preempt another one, and the
-- Scheduler never has to choose between two of them mid-execution.
--
-- Enum ordering follows declaration order and ADD VALUE appends, so
-- 'interrupting' sorts above 'uninterruptible' and the existing GREATEST and
-- >= comparisons keep working unmodified.
--
-- THIS MIGRATION DELIBERATELY CONTAINS NOTHING ELSE.  PostgreSQL will not let a
-- newly added enum value be *used* in the transaction that adds it, and function
-- bodies are validated at CREATE time, so neither V1260's USING clause nor its
-- rewritten view definitions could ride along here.

ALTER TYPE e_execution_requirement ADD VALUE 'interrupting';
