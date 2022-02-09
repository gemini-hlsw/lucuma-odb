
--- PROGRAMS

create domain d_program_id as varchar; -- TODO format check
comment on domain d_program_id is 'GID type for science programs.';

create sequence s_program_id start with 256; -- three hex digits
create table t_program (

  -- identity
  c_program_id  d_program_id not null primary key default 'p-' || to_hex(nextval('s_program_id')),
  c_existence   e_existence  not null default 'present',

  -- -- semester
  -- c_year        smallint check (c_year >= 2004), -- 2021
  -- c_half        d_half   , -- A or B

  -- -- program type
  -- c_type        d_tag    , -- program type

  -- -- band and index in-band
  -- c_band        d_band   , -- 1, 2, 3, 4, or 5 (which means it's a proposal)
  -- c_index       smallint , -- index in-band?

  c_name        text         check (c_name is null or length(c_name) > 0)

);
comment on table t_program is 'Science programs.';


-- User have a role with respect to a given program, distinct from their lucuma role
create type e_program_user_role as enum(
  'pi',         -- "owner" of the science program
  'coi',        -- collaborators with mostly the same access as PIs
  'observer',   -- others who can read but not update
  'support'     -- gemini staff assigned to support this program
);

create table t_program_user (
  c_program_id  d_program_id        not null references t_program(c_program_id) on delete cascade,
  c_user_id     d_user_id           not null references t_user(c_user_id), -- don't cascade .. we shouldn't ever delete a user!
  c_role        e_program_user_role not null,
  unique (c_program_id, c_user_id)
);

create table t_time_allocation (
  c_program_id   d_program_id  not null references t_program(c_program_id) on delete cascade,
  c_partner      d_tag         not null references t_partner(c_tag), -- don't cascade .. we shouldn't ever delete a partner!
  c_duration     interval      not null -- check positive?
);

-- for PI role: select ... from t_collaborator join t_program on t_collaborator.c_program_id = t_program.c_program_id
-- for NGO role: ... from t_time_allocation join ...

insert into t_program (c_program_id, c_name) values ('p-2', 'The real dark matter was the friends we made along the way');
insert into t_program (c_program_id, c_name) values ('p-3', 'An Empty Placeholder Program');

-- trigger to notify when rows are updated

CREATE OR REPLACE FUNCTION ch_program_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  PERFORM pg_notify('ch_program_edit', NEW.c_program_id);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_program_edit_trigger
  AFTER INSERT OR UPDATE ON t_program
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_program_edit();

