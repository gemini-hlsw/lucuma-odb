-- if it's a nonsidereal target then RA, Dec, and Epoch must be defined
alter table t_target
add constraint ra_dec_epoch_all_defined
check (
  c_type = 'nonsidereal' or
  num_nulls(c_sid_ra, c_sid_dec, c_sid_epoch) = 0
);

