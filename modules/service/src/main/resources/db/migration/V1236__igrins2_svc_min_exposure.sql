-- The slit-viewing camera has its own minimum exp time of 1630 ms.

ALTER TABLE t_igrins_2_long_slit
  DROP CONSTRAINT igrins2_svc_exposure_check;

ALTER TABLE t_igrins_2_long_slit
  ADD CONSTRAINT igrins2_svc_exposure_check
    CHECK (c_svc_exposure IS NULL
           OR (c_svc_exposure >= interval '1630 milliseconds'
               AND c_svc_exposure <= interval '600 seconds'));
