create user stockscreenerreports  with encrypted password [enter password here];
create database stockscreenerreports;
grant all privileges on database stockscreenerreports to stockscreenerreports;

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
);

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

drop table industryupdates cascade;

create sequence industrysmabreakdowns_id_seq;
alter table industrysmabreakdowns alter column id set default nextval('industrysmabreakdowns_id_seq');

-- set industrysmabreakdowns_id_seq to be max id value of industrysmabreakdowns
select setval('industrysmabreakdowns_id_seq', (select max(id) from industrysmabreakdowns));

-- set id as primary key of industrysmabreakdowns
alter table industrysmabreakdowns add primary key (id);


create table dailysmabreakdowns (
    id serial primary key,
    "date" timestamp not null,
    above numeric not null,
    below numeric not null,
    days numeric not null,
    UNIQUE(days, "date")
);

create table industrytrends (
    id serial primary key,
    industry text not null,
    "date" timestamp not null,
    streak numeric not null,
    direction text not null,
    change numeric not null,
    days numeric not null,
    UNIQUE(industry, days)
);

create table earnings (
    ticker text not null,
    "date" timestamp not null,
    earningstime text not null,
    primary key (ticker, "date")
);

alter table industrytrends drop constraint industrytrends_industry_days_key;
alter table industrytrends add constraint industrytrends_industry_days_date_key unique (industry, days, "date");

alter table industrytrends add column above numeric;
alter table industrytrends add column below numeric;

update industrytrends set above = (
    select above from industrysmabreakdowns
    where industry = industrytrends.industry
    and days = industrytrends.days
    and "date" = industrytrends."date"
);

update industrytrends set below = (
    select below from industrysmabreakdowns
    where industry = industrytrends.industry
    and days = industrytrends.days
    and "date" = industrytrends."date"
);

-- enforce industrytrends above and below columns to be not null
alter table industrytrends alter column above set not null;
alter table industrytrends alter column below set not null;

-- how to delete a day's worth of data
-- finviz=> delete from dailysmabreakdowns where date = '2023-04-07';
-- finviz=> delete from industrysmabreakdowns where date = '2023-04-07';
-- finviz=> delete from industrytrends where date = '2023-04-07';
-- finviz=> delete from screenerresults where date = '2023-04-07';