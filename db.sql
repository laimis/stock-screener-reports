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
alter table screenerresults add column marketcap numeric;

create table industryupdates (
    id serial primary key,
    industry text not null,
    "date" timestamp not null,
    above numeric not null,
    below numeric not null,
    UNIQUE(industry, "date")
);quit

alter table industryupdates add column days numeric;
UPDATE industryupdates SET days = 20;
alter table industryupdates alter column days set not null;

ALTER TABLE industryupdates DROP CONSTRAINT industryupdates_industry_date_key
ALTER TABLE industryupdates ADD CONSTRAINT industryupdates_industry_date_key UNIQUE (industry, days, "date");

create table jobs (
    id serial primary key,
    name text not null,
    timestamp timestamp with time zone not null,
    status text not null,
    message text not null
);

create table industrysmabreakdowns ( like industryupdates INCLUDING DEFAULTS INCLUDING CONSTRAINTS INCLUDING INDEXES);
insert into industrysmabreakdowns SELECT * FROM industryupdates;