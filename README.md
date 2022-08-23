## Stock Screener Reports portal - Managing your stock screener results made easier

This repo contains code that can be used to run a web portal for managing stock screener results.

It consists of a web application for interacting with the screeners and tickers, a console app that runs screeners (currently based on finviz), and a storage layer for storing the results.

In addition of managing individual ticker results, the site also contains functionality for observing the industries, sectors, and the overall stock market.


### Installation

Currently there is no easy setup available. At a high level, you will need a postgres database for the storage layer. All the db objects should be setup if you run db.sql script. Once you have this setup, you could run the following at the root directory of this project (assuming you have .net installed):

```set SSR_CONNECTIONSTRING=Server^=localhost;Port^=5432;Database^=...;User Id^=...;Password^=...;Include Error Detail=true

dotnet run --project src\StockScreenerReports.Web\StockScreenerReports.Web.fsproj```

This will launch a web project.

Another key component is a nightly screener run. The project includes Finviz based screener tool that you can run:

```dotnet run --project src\FinvizScraper.Console config\config.json --screeners```

Make sure to have config.json configured with the connection string for the database. The tool will look for screeners rows in the database and run it. If you don't have the screener setup, use the web UI to configure it.