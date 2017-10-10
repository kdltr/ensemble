delete from branches;
delete from branches_events;
delete from events;
delete from states;
delete from config where key = 'next-batch';
vacuum;
