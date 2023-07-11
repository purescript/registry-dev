-- migrate:up
create table if not exists jobs (
  jobId text primary key not null,
  jobType text not null,
  createdAt text not null,
  finishedAt text,
  success integer not null default 0
);

-- migrate:down
drop table jobs;
