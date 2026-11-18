-- Time accounting looks up a visit's datasets on every recompute (whether it
-- has any, which fail QA, and whether any science dataset passes), as does
-- visit_time_accounting_locked.  Without an index on the visit each of those
-- scans the whole table.
CREATE INDEX i_dataset_visit ON t_dataset (c_visit_id);
