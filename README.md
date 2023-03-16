## Stock Screener Reports portal - Managing your stock screener results made easier

This repo contains code that can be used to run a web portal for managing stock screener results.

The key components are a web application for interacting with the screener results, a console app that can be used to kick off regular screeners jobs, and storage layer that is used to store the data.

In addition of managing screener results, the portal also provides a way to observe the trends across industries and sectors. There is an earnings section that shows the earning results for the last week and which industries had things like new highs, top movers, new lows, and top losers.

### Installation

Currently there is no easy setup available. At a high level, you will need a postgres database for the storage layer. All the db objects should be setup if you run db.sql script. Once you have this setup, you could run the following at the root directory of this project (assuming you have .net installed):

```set SSR_CONNECTIONSTRING=Server^=localhost;Port^=5432;Database^=...;User Id^=...;Password^=...;Include Error Detail=true

dotnet run --project src\StockScreenerReports.Web\StockScreenerReports.Web.fsproj```

This will launch a web project.

### Populating data

It's up to you to figure out how to push screener results and populate the data into the database. This code base includes a sample screener tool that pulls screener results from finviz.

Once you have the screener URLs configured in the UI, you can kick off the screener job by running the following command:

```dotnet run --project src\StockScreenerReports.Console config\config.json --screeners```

Make sure to have config.json configured with the connection string for the database.