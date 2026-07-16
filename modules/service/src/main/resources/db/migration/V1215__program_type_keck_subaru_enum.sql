-- Add first-class Keck and Subaru program types.
--
-- Exchange (Keck/Subaru) proposals were previously modeled as 'science' programs
-- discriminated by a c_observatory column.  We are promoting them to real program
-- types.  Postgres forbids using a new enum value in the same transaction that
-- adds it, so the ADD VALUE statements live in this migration by themselves; all
-- code that *uses* 'keck'/'subaru' is in the following migration (V1213).
ALTER TYPE e_program_type ADD VALUE IF NOT EXISTS 'keck'   BEFORE 'library';
ALTER TYPE e_program_type ADD VALUE IF NOT EXISTS 'subaru' BEFORE 'system';
