--- TIMING WINDOW ID
create domain d_timing_window_id as varchar; -- TODO format check
comment on domain d_timing_window_id is 'GID for timing windows';
create sequence s_timing_window_id START with 256; -- three hex digits

--- EXISTENCE
create type e_timing_window_inclusion as enum ('include', 'exclude');

-- END AT/AFTER
create type e_timing_window_end_type as enum ('at', 'after');

create table t_timing_window (
  c_timing_window_id d_timing_window_id        primary key default 'w-' || to_hex(nextval('s_timing_window_id')),
  c_observation_id   d_observation_id          not null,
  c_inclusion        e_timing_window_inclusion not null,
  c_start            timestamp                 not null,
  c_end_at           timestamp,  -- if both c_end_at and c_end_after are null, remain open forever
  c_end_after        interval,   --
  c_repeat_period    interval,   -- if null, don't repeat
  c_repeat_times     int4,       -- if null (and c_repeat_period not null), repeat forever

  foreign key (c_observation_id) references t_observation(c_observation_id),

  -- if window ends, either timestamp or duration is defined but not both.
  constraint end_timestamp_nand_interval
  check (num_nulls(c_end_at, c_end_after) >= 1),

  -- repeat criterion may be defined only if duration is defined
  check ( (c_end_after is not null) or (num_nulls(c_repeat_period, c_repeat_times) = 2) ),

  -- c_repeat_times is defined only if c_repeat_period is defined
  check ( (c_repeat_period is not null) or (c_repeat_times is null) )
);

-- a view that has synthetic nullable ids for nullable embedded objects and end type discriminator (required by grackle)
create view v_timing_window as
  select *,
  case
    when num_nulls(c_end_at, c_end_after) < 2 then c_timing_window_id
  end as c_end_id,
  case
    when c_end_at    is not null then 'at'::e_timing_window_end_type
    when c_end_after is not null then 'after'::e_timing_window_end_type
  end as c_end_type,
  case
    when num_nulls(c_repeat_period, c_repeat_times) < 2 then c_timing_window_id
  end as c_repeat_id
  from t_timing_window;