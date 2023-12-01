-- Fix the constraints on the proposal class.

-- We shouldn't have any of these, but just in case...
update t_proposal
   set c_class = 'queue',
       c_min_percent_total = null,
       c_total_time = null
 where (c_class = 'intensive' or c_class = 'large_program')
   and (c_min_percent_total is null or c_total_time is null);

alter table t_proposal
  drop constraint t_proposal_check,
  drop constraint t_proposal_check1,
   add constraint t_proposal_check_min_total 
           check((c_min_percent_total is not null) = (c_class = 'intensive' or c_class = 'large_program')),
   add constraint t_proposal_check_total
           check((c_total_time is not null) = (c_class = 'intensive' or c_class = 'large_program'));
