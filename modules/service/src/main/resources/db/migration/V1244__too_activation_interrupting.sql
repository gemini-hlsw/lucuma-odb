-- Add the most disruptive Target-of-Opportunity activation level.  The ladder
-- becomes, in order of increasing disruption:
--
--   none < standard < rapid < interrupting
--
-- 'rapid' keeps its name (it is what the requirements call "time critical"), so
-- this is a pure append and the existing sort order is preserved -- which
-- matters, because the enum's sort order is what makes MAX() and the ceiling
-- comparison work.
--
-- Part 1 of 2.  This is alone in its own migration because PostgreSQL forbids
-- *using* an enum value in the same transaction that adds it, and Flyway runs
-- each migration in one transaction.  V1241 is the first thing that may
-- reference 'interrupting'.

ALTER TYPE e_too_activation ADD VALUE 'interrupting';
