-- Add an enum to indicate if a target is of calibration role
create type e_calibration_role as enum('twilight', 'photometric', 'spectrophotometric', 'telluric');
comment on type e_calibration_role is 'The role of the target in a calibration.';

alter table t_target
drop column c_role cascade;

alter table t_target
add column c_calibration_role e_calibration_role;
