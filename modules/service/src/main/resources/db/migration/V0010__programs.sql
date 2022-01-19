
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

  c_name        text         not null default 'Untitled Science Program'

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
  c_program_id  d_program_id        not null references t_program(c_program_id),
  c_user_id     d_user_id           not null references t_user(c_user_id),
  c_role        e_program_user_role not null,
  unique (c_program_id, c_user_id)
);

create table t_time_allocation (
  c_program_id   d_program_id  not null references t_program(c_program_id),
  c_partner      d_tag         not null references t_partner(c_tag),
  c_duration     interval      not null -- check positive?
)

-- for PI role: select ... from t_collaborator join t_program on t_collaborator.c_program_id = t_program.c_program_id
-- for NGO role: ... from t_time_allocation join ...


