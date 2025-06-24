

-- Add new enum case
alter type e_target_type add value 'opportunity';

-- Add enum tag type for arc types
create type e_arc_type as enum('empty', 'full', 'partial');
