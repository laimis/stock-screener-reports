create user finviz  with encrypted password [enter password here];
create database finviz;
grant all privileges on database finviz to finviz;

create table stocks (
  id serial primary key,
  ticker text not null,
  name text not null,
  sector text not null,
  industry text not null,
  country text not null
);

create unique index ON stocks USING BTREE(ticker);

create table screeners (
    id serial primary key,
    name text not null,
    url text not null
);

create unique index ON screeners USING BTREE(name);

create table screenerresults (
    id serial primary key,
    screenerid int not null,
    stockid int not null,
    "date" timestamp not null,
    price decimal not null,
    UNIQUE(screenerid, stockid, "date"),
    constraint fk_screenerresults_screenerid FOREIGN KEY(screenerid) REFERENCES screeners(id),
    constraint fk_screenerresults_stockid FOREIGN KEY(stockid) REFERENCES stocks(id)
);

alter table screenerresults add column change decimal;
alter table screenerresults add column volume numeric;
