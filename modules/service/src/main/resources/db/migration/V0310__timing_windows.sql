--- EXISTENCE
CREATE TYPE e_timing_window_inclusion AS ENUM ('include', 'exclude');

-- END AT/AFTER
CREATE TYPE e_timing_window_end_type AS ENUM ('at', 'after');

CREATE TABLE t_timing_window (
  c_timing_window_id bigserial                 PRIMARY KEY,
  c_observation_id   d_observation_id          NOT NULL,
  c_inclusion        e_timing_window_inclusion NOT NULL,
  c_start            timestamp                 NOT NULL,
  c_end_at           timestamp,  -- if both c_end_at and c_end_after are null, remain open forever
  c_end_after        interval,   --
  c_repeat_period    interval,   -- if null, don't repeat
  c_repeat_times     int4,       -- if null (and c_repeat_period not null), repeat forever

  foreign key (c_observation_id) references t_observation(c_observation_id),

  -- if window ends, either timestamp or duration is defined but not both.
  constraint end_at_nand_end_after
  check (num_nulls(c_end_at, c_end_after) >= 1),

  -- repeat criterion is defined only if c_end_after is defined
  constraint repeat_only_if_end_after
  check ( (c_end_after is not null) or (num_nulls(c_repeat_period, c_repeat_times) = 2) ),

  -- c_repeat_times is defined only if c_repeat_period is defined
  constraint repeat_times_only_if_repeat_period
  check ( (c_repeat_period is not null) or (c_repeat_times is null) ),

  -- c_end_at is greater than c_start
  constraint c_end_at_greater_than_c_start
  check ( c_end_at > c_start ),

  -- c_repeat_period is greater than c_end_after
  constraint repeat_period_greater_than_end_after
  check ( c_repeat_period > c_end_after )
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