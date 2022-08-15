
--- PROGRAMS

create domain d_program_id as varchar; -- TODO format check
comment on domain d_program_id is 'GID type for science programs.';

create sequence s_program_id start with 256; -- three hex digits
create table t_program (

  -- identity
  c_program_id  d_program_id not null primary key default 'p-' || to_hex(nextval('s_program_id')),
  c_existence   e_existence  not null default 'present',

  -- principal investigator (composite fk here, see below)
  c_pi_user_id    d_user_id,
  c_pi_user_type  e_user_type check (c_pi_user_type = 'guest' or c_pi_user_type = 'standard'),

  -- -- semester
  -- c_year        smallint check (c_year >= 2004), -- 2021
  -- c_half        d_half   , -- A or B

  -- -- program type
  -- c_type        d_tag    , -- program type

  -- -- band and index in-band
  -- c_band        d_band   , -- 1, 2, 3, 4, or 5 (which means it's a proposal)
  -- c_index       smallint , -- index in-band?

  c_name        text         check (c_name is null or length(c_name) > 0),

  -- planned time summary
  c_pts_pi        interval not null default '0h',
  c_pts_uncharged interval not null default '0h',
  c_pts_execution interval not null default '0h',

  -- We link back to the user table thus.
  FOREIGN KEY (c_pi_user_id, c_pi_user_type)
  REFERENCES t_user (c_user_id, c_user_type)

);
comment on table t_program is 'Science programs.';


-- User have a role with respect to a given program, distinct from their lucuma role
create type e_program_user_role as enum(
  'coi',        -- collaborators with mostly the same access as PIs
  'observer',   -- others who can read but not update
  'support'     -- gemini or ngo staff assigned to support this program
);

-- User have a role with respect to a given program, distinct from their lucuma role
create type e_program_user_support_type as enum(
  'staff',      -- staff support
  'partner'     -- partner support
);

create table t_program_user (
  c_program_id      d_program_id                 not null references t_program(c_program_id) on delete cascade,
  c_user_id         d_user_id                    not null references t_user(c_user_id), -- don't cascade .. we shouldn't ever delete a user!
  c_user_type       e_user_type                  check (c_user_type = 'standard'), -- only standard users
  c_role            e_program_user_role          not null,
  c_support_type    e_program_user_support_type  default null,
  c_support_partner d_tag                        default null references t_partner(c_tag), -- check constraint below
  unique (c_program_id, c_user_id),
  check (
    (c_support_type = 'partner' and c_support_partner is not null) or
    (c_support_partner is null)
  ),

    -- We link back to the user table thus.
  FOREIGN KEY (c_user_id, c_user_type)
  REFERENCES t_user (c_user_id, c_user_type)

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

CREATE UNLOGGED TABLE t_program_event (
  c_event_id     bigint not null default nextval('s_event_id'), -- primary key, incrementing
  c_edit_type    e_edit_type  not null,
  c_program_id   d_program_id  not null references t_program(c_program_id) on delete cascade
);

CREATE OR REPLACE FUNCTION ch_program_edit()
  RETURNS trigger AS $$
DECLARE
BEGIN
  IF (TG_OP = 'INSERT') THEN
    INSERT INTO t_program_event (c_edit_type, c_program_id)
    VALUES ('created', NEW.c_program_id);
    PERFORM pg_notify('ch_program_edit', NEW.c_program_id || ',' || currval('s_event_id')::text);
  ELSIF (TG_OP = 'UPDATE') THEN
    INSERT INTO t_program_event (c_edit_type, c_program_id)
    VALUES ('updated', NEW.c_program_id);
    PERFORM pg_notify('ch_program_edit',  NEW.c_program_id || ',' || currval('s_event_id')::text);
  END IF;
  RETURN NEW; -- n.b. doesn't matter, it's an AFTER trigger
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER ch_program_edit_trigger
  AFTER INSERT OR UPDATE ON t_program
  DEFERRABLE
  FOR EACH ROW
  EXECUTE PROCEDURE ch_program_edit();

