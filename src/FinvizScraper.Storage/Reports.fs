namespace FinvizScraper.Storage

module Reports =

    open Npgsql.FSharp
    open System

    let mutable private cnnString = ""

    let configureConnectionString str =
        cnnString <- str

    type ScreenerResultReport =
        {
            screenerid:int;
            name:string;
            url:string;
            date:DateTime;
            count:int
        }
    type ScreenerResultReportItem =
        {
            stockid:int;
            ticker:string;
            name:string;
            sector:string;
            industry:string;
            country:string;
            date:DateTime;
            marketCap:decimal;
            price:decimal;
            change:decimal;
            volume:int;
            screenerid:int;
            screenername:string;
        }

    let private mapScreenerResultReportItem (reader:RowReader) =
        {
            stockid = (reader.int "id");
            ticker = (reader.string "ticker");
            name = (reader.string "name");
            sector = (reader.string "sector");
            industry = (reader.string "industry");
            country = (reader.string "country");
            date = (reader.dateTime "date");
            marketCap = (
                reader.decimalOrNone "marketcap"
                |> Option.defaultValue 0m
            );
            price = (reader.decimal "price");
            change = (reader.decimal "change");
            volume = (reader.int "volume");
            screenerid = (reader.int "screenerid");
            screenername = (reader.string "screenername");
        }

    let private topGrouping screenerId date grouping =
        let sql = @$"SELECT {grouping},count(*) as count FROM stocks
            JOIN screenerresults ON stocks.id = screenerresults.stockid
            WHERE 
                screenerresults.screenerid = @screenerId
                AND screenerresults.date = @date
            GROUP BY {grouping}
            ORDER BY count DESC"

        let results =
            cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@date", Sql.timestamp date;
                "@screenerId", Sql.int screenerId
            ]
            |> Sql.execute (fun reader ->
                (reader.string grouping, reader.int "count")
            )

        results

    let private topGroupingOverDays screenerId fromDate toDate grouping =
        let sql = @$"SELECT {grouping},count(*) as count FROM stocks
            JOIN screenerresults ON stocks.id = screenerresults.stockid
            WHERE 
                screenerresults.screenerid = @screenerId
                AND screenerresults.date >= @fromDate
                AND screenerresults.date <= @toDate
            GROUP BY {grouping}
            ORDER BY count DESC"

        let results =
            cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@fromDate", Sql.timestamp fromDate;
                "@toDate", Sql.timestamp toDate;
                "@screenerId", Sql.int screenerId
            ]
            |> Sql.execute (fun reader ->
                (reader.string grouping, reader.int "count")
            )

        results

    let topSectors screenerId date =
        "sector" |> topGrouping screenerId date

    let topIndustries screenerId date =
        "industry" |> topGrouping screenerId date

    let topCountries screenerId date =
        "country" |> topGrouping screenerId date

    let topSectorsOverDays scrennerId startDate endDate =
        "sector" |> topGroupingOverDays scrennerId startDate endDate

    let topIndustriesOverDays scrennerId startDate endDate =
        "industry" |> topGroupingOverDays scrennerId startDate endDate

    let topCountriesOverDays scrennerId startDate endDate =
        "country" |> topGroupingOverDays scrennerId startDate endDate

    let getLatestScreeners() =
        let sql = @$"SELECT screenerid,name,url,date,count(*) as count FROM screenerresults
            JOIN screeners ON screenerresults.screenerid = screeners.id
            WHERE date = (SELECT MAX(date) FROM screenerresults)
            GROUP BY screenerid,name,url,date
            ORDER BY screenerid"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.execute (fun reader ->
                {
                    screenerid = (reader.int "screenerid");
                    name = (reader.string "name");
                    url = (reader.string "url");
                    date = (reader.dateTime "date");
                    count = (reader.int "count")
                }
            )

    let getScreenerResults id date =

        let sql = @$"
            SELECT 
                stocks.id,ticker,stocks.name,sector,industry,country,
                screeners.id as screenerid,screeners.name as screenername,
                screenerresults.date,marketcap,price,change,volume
            FROM stocks
            JOIN screenerresults ON stocks.id = screenerresults.stockid
            JOIN screeners ON screeners.id = screenerresults.screenerid
            WHERE 
                screenerresults.screenerid = @screenerid
                AND screenerresults.date = date(@date)
            ORDER BY screenerresults.volume DESC"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@date", Sql.string date;
                "@screenerid", Sql.int id
            ]
            |> Sql.execute (fun reader -> mapScreenerResultReportItem reader)

    let getDailyCountsForScreener id days =

        let sql = @$"
            SELECT 
                date,count(*) as count
            FROM screenerresults
            WHERE 
                screenerid = @screenerid
                AND date >= current_date - @days
            GROUP BY date
            ORDER BY date"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@screenerid", Sql.int id;
                "@days", Sql.int days
            ]
            |> Sql.execute (fun reader -> 
                (
                    reader.dateTime "date",
                    reader.int "count"
                )
            )

    let getScreenerResultsForTicker (ticker:FinvizScraper.Core.StockTicker.T) =
            
        let sql = @$"
            SELECT 
                stocks.id,ticker,stocks.name,sector,industry,country,
                screeners.id as screenerid,screeners.name as screenername,
                screenerresults.date,marketcap,price,change,volume
            FROM stocks
            JOIN screenerresults ON stocks.id = screenerresults.stockid
            JOIN screeners ON screeners.id = screenerresults.screenerid
            WHERE 
                stocks.ticker = @ticker
            ORDER BY screenerresults.date DESC"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@ticker", ticker |> FinvizScraper.Core.StockTicker.value |> Sql.string
            ]
            |> Sql.execute (
                fun reader -> mapScreenerResultReportItem reader
            )