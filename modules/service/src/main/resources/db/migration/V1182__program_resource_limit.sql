-- Cap the number of user-created resources (observations + groups + targets +
-- attachments + program notes, combined) that may be associated with a single
-- program. This guards the public API against runaway scripts and malicious
-- bulk creation.
--
-- Design:
--   * The running count is kept in a dedicated table t_program_resource_count,
--     NOT on t_program. This is deliberate: child rows FK-reference t_program,
--     so concurrent inserts take FOR KEY SHARE locks on the t_program row; a
--     counter UPDATE on t_program (FOR NO KEY UPDATE) then deadlocks under
--     concurrency (mutual lock upgrade). Nothing FK-references the count table,
--     so the counter UPDATE there only serializes -- no deadlock. It also keeps
--     t_program unmodified, so resource changes don't fire the program edit /
--     chronicle triggers.
--   * Maintenance is O(1): a single immediate AFTER trigger per contributing
--     table adjusts the count by +/- 1.
--   * Enforcement is a DEFERRABLE INITIALLY DEFERRED constraint trigger so the
--     check runs against the final state at commit (allows e.g. bulk
--     insert-then-delete in one transaction).
--
-- "Counts toward the limit" means c_existence = 'present' (where the table has
-- it) AND the row is not a system/calibration object. System-generated objects
-- (calibration observations/targets, system groups) are exempt, as are
-- system/calibration *programs* themselves. Attachments have no existence or
-- calibration columns, so every attachment counts; program notes count when
-- present.

-- The per-program limit lives on t_program (set explicitly by staff, rarely).
alter table t_program
  add column c_resource_limit integer not null default 1000 check (c_resource_limit >= 0);

comment on column t_program.c_resource_limit is
  'Maximum combined resource count (observations, groups, targets, attachments, program notes) for this program. Defaults to 1000; raisable by staff.';

-- The running count, kept off t_program so resource changes never lock or
-- modify the program row.
create table t_program_resource_count (
  c_program_id     d_program_id not null primary key references t_program(c_program_id) on delete cascade,
  c_resource_count integer      not null default 0 check (c_resource_count >= 0)
);

-- The true count, recomputed from the contributing tables. Used to backfill and
-- to reconcile/verify the maintained counter.
create or replace function program_resource_count_actual(pid d_program_id)
returns integer as $$
  select
    coalesce((select count(*) from t_observation o
              where o.c_program_id = pid and o.c_existence = 'present' and o.c_calibration_role is null), 0)
  + coalesce((select count(*) from t_group g
              where g.c_program_id = pid and g.c_existence = 'present' and g.c_system = false), 0)
  + coalesce((select count(*) from t_target t
              where t.c_program_id = pid and t.c_existence = 'present' and t.c_calibration_role is null), 0)
  + coalesce((select count(*) from t_program_note n
              where n.c_program_id = pid and n.c_existence = 'present'), 0)
  + coalesce((select count(*) from t_attachment a
              where a.c_program_id = pid), 0);
$$ language sql stable;

-- Seed a count row whenever a program is created.
create or replace function init_program_resource_count()
returns trigger as $$
begin
  insert into t_program_resource_count (c_program_id, c_resource_count)
  values (NEW.c_program_id, 0);
  return NEW;
end;
$$ language plpgsql;

create trigger init_program_resource_count_trigger
after insert on t_program
for each row execute function init_program_resource_count();

-- Backfill existing programs.
insert into t_program_resource_count (c_program_id, c_resource_count)
  select c_program_id, program_resource_count_actual(c_program_id) from t_program;

-- Apply a delta to a program's maintained counter. Locks only the count row,
-- which nothing else contends for, so concurrent inserts serialize here without
-- deadlocking.
create or replace function adjust_program_resource_count(pid d_program_id, delta integer)
returns void as $$
begin
  update t_program_resource_count
     set c_resource_count = c_resource_count + delta
   where c_program_id = pid;
end;
$$ language plpgsql;

-- ===========================================================================
-- Counter maintenance (immediate). One trigger function shared by all
-- contributing tables; the "counts toward the limit" predicate is selected on
-- TG_TABLE_NAME (PL/pgSQL plans each branch lazily, so a column is only
-- referenced for the table that has it). Handles soft-delete, restore, program
-- moves, and role changes.
-- ===========================================================================
create or replace function update_program_resource_count()
returns trigger as $$
declare
  old_counts boolean := false;
  new_counts boolean := false;
begin
  case TG_TABLE_NAME
    when 't_attachment' then
      old_counts := TG_OP in ('UPDATE', 'DELETE');
      new_counts := TG_OP in ('UPDATE', 'INSERT');
    when 't_group' then
      if TG_OP in ('UPDATE', 'DELETE') then old_counts := (OLD.c_existence = 'present' and OLD.c_system = false); end if;
      if TG_OP in ('UPDATE', 'INSERT') then new_counts := (NEW.c_existence = 'present' and NEW.c_system = false); end if;
    when 't_program_note' then
      if TG_OP in ('UPDATE', 'DELETE') then old_counts := (OLD.c_existence = 'present'); end if;
      if TG_OP in ('UPDATE', 'INSERT') then new_counts := (NEW.c_existence = 'present'); end if;
    else -- t_observation, t_target
      if TG_OP in ('UPDATE', 'DELETE') then old_counts := (OLD.c_existence = 'present' and OLD.c_calibration_role is null); end if;
      if TG_OP in ('UPDATE', 'INSERT') then new_counts := (NEW.c_existence = 'present' and NEW.c_calibration_role is null); end if;
  end case;

  if TG_OP = 'UPDATE' and old_counts and new_counts
     and OLD.c_program_id = NEW.c_program_id then
    return NEW;
  end if;

  if old_counts then perform adjust_program_resource_count(OLD.c_program_id, -1); end if;
  if new_counts then perform adjust_program_resource_count(NEW.c_program_id,  1); end if;

  if TG_OP = 'DELETE' then return OLD; else return NEW; end if;
end;
$$ language plpgsql;

create trigger update_program_resource_count_on_observation
after insert or delete or update of c_program_id, c_existence, c_calibration_role on t_observation
for each row execute function update_program_resource_count();

create trigger update_program_resource_count_on_group
after insert or delete or update of c_program_id, c_existence, c_system on t_group
for each row execute function update_program_resource_count();

create trigger update_program_resource_count_on_target
after insert or delete or update of c_program_id, c_existence, c_calibration_role on t_target
for each row execute function update_program_resource_count();

create trigger update_program_resource_count_on_program_note
after insert or delete or update of c_program_id, c_existence on t_program_note
for each row execute function update_program_resource_count();

create trigger update_program_resource_count_on_attachment
after insert or delete or update of c_program_id on t_attachment
for each row execute function update_program_resource_count();

-- ===========================================================================
-- Limit enforcement (deferred). Reads the maintained count and the per-program
-- limit; raises a custom SQLSTATE so the service layer can distinguish it from
-- other RAISEs (which use the default P0001). Only an addition can violate the
-- limit, so we check on INSERT/UPDATE only. System/calibration programs are
-- exempt.
-- ===========================================================================
create or replace function check_program_resource_limit()
returns trigger as $$
declare
  cnt       integer;
  lim       integer;
  is_system boolean;
begin
  select c_resource_limit, c_calibration_role is not null
    into lim, is_system
    from t_program
   where c_program_id = NEW.c_program_id;

  select c_resource_count into cnt
    from t_program_resource_count
   where c_program_id = NEW.c_program_id;

  if not is_system and cnt > lim then
    raise exception
      'Program % cannot have more than % associated resources (observations, groups, targets, attachments, and program notes combined).',
      NEW.c_program_id, lim
      using errcode = 'LU001';
  end if;

  return NEW;
end;
$$ language plpgsql;

create constraint trigger check_program_resource_limit_on_observation
after insert or update of c_program_id, c_existence, c_calibration_role on t_observation
deferrable initially deferred
for each row execute function check_program_resource_limit();

create constraint trigger check_program_resource_limit_on_group
after insert or update of c_program_id, c_existence, c_system on t_group
deferrable initially deferred
for each row execute function check_program_resource_limit();

create constraint trigger check_program_resource_limit_on_target
after insert or update of c_program_id, c_existence, c_calibration_role on t_target
deferrable initially deferred
for each row execute function check_program_resource_limit();

create constraint trigger check_program_resource_limit_on_program_note
after insert or update of c_program_id, c_existence on t_program_note
deferrable initially deferred
for each row execute function check_program_resource_limit();

create constraint trigger check_program_resource_limit_on_attachment
after insert or update of c_program_id on t_attachment
deferrable initially deferred
for each row execute function check_program_resource_limit();

-- Expose the maintained count alongside the program columns.
create view v_program as
  select p.*, coalesce(rc.c_resource_count, 0) as c_resource_count
  from t_program p
  left join t_program_resource_count rc on rc.c_program_id = p.c_program_id;
