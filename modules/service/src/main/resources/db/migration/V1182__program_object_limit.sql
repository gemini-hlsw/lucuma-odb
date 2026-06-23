-- Cap the number of user-created objects (observations + groups + targets,
-- combined) that may be associated with a single program. This guards the
-- public API against runaway scripts and malicious bulk creation.
--
-- Strategy:
--   * Maintain a running c_object_count on t_program via immediate, row-locked
--     AFTER triggers on the three tables. The row lock on t_program serializes
--     concurrent inserts for the same program, so the count stays exact under
--     concurrency and is cheap (O(1)) to read.
--   * Enforce the limit with a DEFERRABLE INITIALLY DEFERRED constraint trigger
--     so the check runs against the final state at commit (allows e.g. bulk
--     insert-then-delete in one transaction). This mirrors the existing
--     check_telluric_group_observations pattern in V1070.
--
-- "Counts toward the limit" means: c_existence = 'present' AND the row is not a
-- system/calibration object. System-generated objects (calibration
-- observations/targets, system groups) are exempt, as are system/calibration
-- *programs* themselves.

-- The per-program object count and an optional per-program override.
-- c_max_objects NULL => use the system default (see program_object_limit).
alter table t_program
  add column c_object_count integer not null default 0,
  add column c_max_objects  integer null check (c_max_objects is null or c_max_objects >= 0);

comment on column t_program.c_object_count is
  'Running count of present, non-system observations + groups + targets in this program.';
comment on column t_program.c_max_objects is
  'Optional per-program override for the maximum combined object count. NULL uses the system default.';

-- Effective limit for a program: the per-program override, or the default.
-- The default cap (1000) lives here and nowhere else; change it to tune the
-- global default.
create or replace function program_object_limit(pid d_program_id)
returns integer as $$
  select coalesce(c_max_objects, 1000)
  from t_program
  where c_program_id = pid;
$$ language sql stable;

-- The true count, recomputed from the three tables. Used to backfill and to
-- reconcile/verify the maintained counter (counters can drift if a transition
-- is ever missed).
create or replace function program_object_count_actual(pid d_program_id)
returns integer as $$
  select
    coalesce((select count(*) from t_observation o
              where o.c_program_id = pid
                and o.c_existence = 'present'
                and o.c_calibration_role is null), 0)
  + coalesce((select count(*) from t_group g
              where g.c_program_id = pid
                and g.c_existence = 'present'
                and g.c_system = false), 0)
  + coalesce((select count(*) from t_target t
              where t.c_program_id = pid
                and t.c_existence = 'present'
                and t.c_calibration_role is null), 0);
$$ language sql stable;

-- Apply a delta to a program's maintained counter. The UPDATE takes a row lock
-- on the program, serializing concurrent object inserts for that program.
create or replace function adjust_program_object_count(pid d_program_id, delta integer)
returns void as $$
begin
  update t_program
     set c_object_count = c_object_count + delta
   where c_program_id = pid;
end;
$$ language plpgsql;

-- ===========================================================================
-- Counter maintenance (immediate).
--
-- A single trigger function shared by all three tables. The only thing that
-- varies is the "counts toward the limit" predicate, which we select on
-- TG_TABLE_NAME. PL/pgSQL plans each statement lazily on first execution, so
-- the c_system reference is only ever planned for t_group and the
-- c_calibration_role reference only for the other two tables; no dynamic SQL is
-- needed. Handles soft-delete, restore, program moves, and role changes.
-- ===========================================================================
create or replace function update_program_object_count()
returns trigger as $$
declare
  old_counts boolean := false;
  new_counts boolean := false;
begin
  if TG_TABLE_NAME = 't_group' then
    if TG_OP in ('UPDATE', 'DELETE') then
      old_counts := (OLD.c_existence = 'present' and OLD.c_system = false);
    end if;
    if TG_OP in ('UPDATE', 'INSERT') then
      new_counts := (NEW.c_existence = 'present' and NEW.c_system = false);
    end if;
  else -- t_observation, t_target
    if TG_OP in ('UPDATE', 'DELETE') then
      old_counts := (OLD.c_existence = 'present' and OLD.c_calibration_role is null);
    end if;
    if TG_OP in ('UPDATE', 'INSERT') then
      new_counts := (NEW.c_existence = 'present' and NEW.c_calibration_role is null);
    end if;
  end if;

  -- No net change within the same program: nothing to do.
  if TG_OP = 'UPDATE' and old_counts and new_counts
     and OLD.c_program_id = NEW.c_program_id then
    return NEW;
  end if;

  if old_counts then perform adjust_program_object_count(OLD.c_program_id, -1); end if;
  if new_counts then perform adjust_program_object_count(NEW.c_program_id,  1); end if;

  if TG_OP = 'DELETE' then return OLD; else return NEW; end if;
end;
$$ language plpgsql;

create trigger update_program_object_count_on_observation
after insert or delete or update of c_program_id, c_existence, c_calibration_role on t_observation
for each row execute function update_program_object_count();

create trigger update_program_object_count_on_group
after insert or delete or update of c_program_id, c_existence, c_system on t_group
for each row execute function update_program_object_count();

create trigger update_program_object_count_on_target
after insert or delete or update of c_program_id, c_existence, c_calibration_role on t_target
for each row execute function update_program_object_count();

-- ===========================================================================
-- Limit enforcement (deferred). Runs at commit against the final counter.
-- Only an addition can violate the limit, so we check on INSERT/UPDATE only.
-- System/calibration programs are exempt. Uses a custom SQLSTATE so the service
-- layer can distinguish this from other RAISEs (which use P0001).
-- ===========================================================================
create or replace function check_program_object_limit()
returns trigger as $$
declare
  cnt       integer;
  is_system boolean;
begin
  select c_object_count, c_calibration_role is not null
    into cnt, is_system
    from t_program
   where c_program_id = NEW.c_program_id;

  if not is_system and cnt > program_object_limit(NEW.c_program_id) then
    raise exception
      'Program % cannot have more than % objects (observations, groups, and targets combined).',
      NEW.c_program_id, program_object_limit(NEW.c_program_id)
      using errcode = 'LU001';
  end if;

  return NEW;
end;
$$ language plpgsql;

create constraint trigger check_program_object_limit_on_observation
after insert or update of c_program_id, c_existence, c_calibration_role on t_observation
deferrable initially deferred
for each row execute function check_program_object_limit();

create constraint trigger check_program_object_limit_on_group
after insert or update of c_program_id, c_existence, c_system on t_group
deferrable initially deferred
for each row execute function check_program_object_limit();

create constraint trigger check_program_object_limit_on_target
after insert or update of c_program_id, c_existence, c_calibration_role on t_target
deferrable initially deferred
for each row execute function check_program_object_limit();

-- ===========================================================================
-- Backfill the counter for existing programs.
-- ===========================================================================
update t_program p
   set c_object_count = program_object_count_actual(p.c_program_id);
