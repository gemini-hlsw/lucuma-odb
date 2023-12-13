-- Stored time accounting values.  These are calculated on demand and stored
-- but reset whenever new events for a visit appear.
CREATE TABLE t_time_accounting (
  c_visit_id               d_visit_id  PRIMARY KEY REFERENCES t_visit(c_visit_id) ON DELETE CASCADE,

  c_raw_non_charged_time   interval(6) NOT NULL DEFAULT '0'::interval
    CHECK (c_raw_non_charged_time >= '0'::interval),

  c_raw_partner_time       interval(6) NOT NULL DEFAULT '0'::interval
    CHECK (c_raw_partner_time >= '0'::interval),

  c_raw_program_time       interval(6) NOT NULL DEFAULT '0'::interval
    CHECK (c_raw_program_time >= '0'::interval),

  c_final_non_charged_time interval(6) NOT NULL DEFAULT '0'::interval
    CHECK (c_final_non_charged_time >= '0'::interval),

  c_final_partner_time     interval(6) NOT NULL DEFAULT '0'::interval
    CHECK (c_final_partner_time >= '0'::interval),

  c_final_program_time     interval(6) NOT NULL DEFAULT '0'::interval
    CHECK (c_final_program_time >= '0'::interval)

);