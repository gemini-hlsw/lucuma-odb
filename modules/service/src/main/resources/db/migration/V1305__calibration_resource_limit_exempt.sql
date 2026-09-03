-- Only check the resource limit when the row being written actually counts
-- toward it.
--
-- The counter maintained by update_program_resource_count() already ignores
-- calibration and soft-deleted rows, but the limit check did not: it compared
-- the program's total against the limit no matter what fired it. So a program
-- sitting at its limit rejected every write, even writes that add nothing.
-- Two things broke as a result: soft deletes, so a program could not shrink
-- back under the limit, and calibration recalculation, because
-- CalibrationsService inserts calibration observations with a null
-- c_calibration_role and fills it in before commit.
--
-- The predicate below is the same one the counter uses. The check is deferred
-- to commit, so it sees the row's final state (including that filled-in
-- calibration role). A row that does not count cannot be what pushed the
-- program over.
create or replace function check_program_resource_limit()
returns trigger as $$
declare
  cnt        integer;
  lim        integer;
  is_system  boolean;
  new_counts boolean;
begin
  case TG_TABLE_NAME
    when 't_attachment'   then new_counts := true;
    when 't_group'        then new_counts := (NEW.c_existence = 'present' and NEW.c_system = false);
    when 't_program_note' then new_counts := (NEW.c_existence = 'present');
    else -- t_observation, t_target
      new_counts := (NEW.c_existence = 'present' and NEW.c_calibration_role is null);
  end case;

  if not new_counts then
    return NEW;
  end if;

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
