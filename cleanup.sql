delete from branches;
delete from events;
delete from states;
delete from config where key = 'next-batch';
vacuum;
