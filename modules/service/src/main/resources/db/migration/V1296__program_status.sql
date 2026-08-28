-- The Program Status

create table t_program_status (
  c_tag       d_tag      not null primary key,
  c_name      varchar    not null,
  c_ordinal   smallint   not null unique
);

insert into t_program_status (c_tag, c_name, c_ordinal) values ('active', 'Active', 0);
insert into t_program_status (c_tag, c_name, c_ordinal) values ('inactive', 'Inactive', 1);
insert into t_program_status (c_tag, c_name, c_ordinal) values ('complete', 'Complete', 2);
insert into t_program_status (c_tag, c_name, c_ordinal) values ('incomplete', 'Incomplete', 3);

-- v_program expands t_program.* at creation time, so it must be dropped and
-- recreated to pick up the new column.
drop view v_program;

alter table t_program
add column c_program_status d_tag not null default 'active' references t_program_status(c_tag);

create view v_program as
  select
    p.*,
    coalesce(rc.c_resource_count, 0) as c_resource_count,
    ((now() at time zone 'UTC')::date between p.c_active_start and p.c_active_end) as c_is_active
  from t_program p
  left join t_program_resource_count rc on rc.c_program_id = p.c_program_id;
