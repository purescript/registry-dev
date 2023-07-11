-- migrate:up
create table if not exists logs (
  id integer primary key autoincrement,
  jobId text not null references jobs,
  level integer not null,
  message text not null,
  timestamp text not null
);

-- migrate:down
drop table logs;
