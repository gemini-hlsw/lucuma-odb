-- Takes the UNIQUE constraints off t_program.c_program_reference and
-- c_proposal_reference, which were forcing every update of a program to take
-- FOR UPDATE.  Same class as V1270 and V1279, one table up.
--
-- WHY THEY ESCALATE
--
-- A BEFORE ... FOR EACH ROW UPDATE trigger (here update_program_type, V0882) has
-- to be handed the row it fires on, so Postgres locks that row before running
-- it -- before the new values exist to compare against the old ones.  So it
-- picks the lock mode from the statement's target columns instead: if any of
-- them is in a unique index, it takes the strong lock.  A stored generated
-- column is always targeted, since it must be recomputed on every update.  So
-- these two put a key column in the target list of every UPDATE of t_program,
-- whatever the statement sets and whatever the reference's value is.
--
-- That blocks the FOR KEY SHARE Postgres takes on t_program for the foreign key
-- of every child row inserted (V1227): renaming a program or submitting a
-- proposal blocks concurrent inserts of observations, targets, groups and
-- allocations in it, and the reverse.
--
-- WHAT REPLACES THEM
--
-- c_proposal_reference is unique by construction, so its UNIQUE simply goes: it
-- is non-null only for science/keck/subaru, which share one nextval() sequence
-- per semester (next_semester_index, V0949).
--
-- c_program_reference is not.  Its example/library/system branches are built
-- from user-supplied fields with nothing else pinning them, and
-- setProgramReference reports duplicates by catching the violation.  So the
-- constraint moves onto the same expression: a unique index over an expression
-- has no heap attribute to contribute, so it is invisible to the target-column
-- check above, while enforcing exactly what it enforced before.
--
-- Neither column had any other index and both are looked up by value
-- (ProgramService.selectPid), so each also gets a plain one -- what V1270 and
-- V1279 already had and could simply keep.
--
-- The trade: an expression index blocks HOT updates by column rather than by
-- value, so touching any input now costs a non-HOT update even when the
-- reference is unchanged (measured 500/500 HOT before, 0/500 after).  Only
-- setProgramReference and proposal edits write those columns, and most of those
-- changed the reference and were non-HOT anyway; c_name, c_description,
-- c_active_start/end and the rest touch none of them and stay fully HOT.  At 435
-- rows this is noise.
--
-- This does not weaken the guard in ArchiveDuplicationSearchService, which takes
-- FOR NO KEY UPDATE on the program to serialize against a submission landing
-- mid-call: FOR NO KEY UPDATE conflicts with itself, so submission still waits.

ALTER TABLE t_program
  DROP CONSTRAINT t_program_c_proposal_reference_key,
  DROP CONSTRAINT t_program_c_program_reference_key;

CREATE UNIQUE INDEX i_program_reference_unique ON t_program (
  format_program_reference(
    c_program_type,
    c_semester,
    c_semester_index,
    c_proposal_status,
    c_science_subtype,
    c_instrument,
    c_library_desc,
    c_subaru_proposal_type
  )
);

CREATE INDEX i_program_reference  ON t_program (c_program_reference);
CREATE INDEX i_proposal_reference ON t_program (c_proposal_reference);

COMMENT ON COLUMN t_program.c_program_reference IS
  'Generated from the program type and its reference-defining fields. Unique, '
  'but enforced by i_program_reference_unique over the generating expression '
  'rather than by a constraint on the column (see V1280): a generated column in '
  'a unique index forces every update of the row to take FOR UPDATE, which '
  'blocks the FK lock every child-row insert takes on the program.';

COMMENT ON COLUMN t_program.c_proposal_reference IS
  'Generated from the program type, semester and semester index. Unique by '
  'construction rather than by constraint (see V1280): the semester index comes '
  'from a per-semester sequence shared by all proposal-bearing program types.';
