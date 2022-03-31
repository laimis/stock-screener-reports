namespace FinvizScraper.Storage

module Reports =

    open Npgsql.FSharp
    open System
    open FinvizScraper.Core

    let mutable private cnnString = ""

    let configureConnectionString str =
        cnnString <- str

    type ScreenerResultReportItem =
        {
            stockid:int;
            ticker:string;
            name:string;
            sector:string;
            industry:string;
            country:string;
            price:decimal;
            change:decimal;
            volume:int;
        }

    let private topGrouping screener days grouping =
        let sql = @$"SELECT {grouping},count(*) as count FROM stocks
            JOIN screenerresults ON stocks.id = screenerresults.stockid
            WHERE 
                screenerresults.screenerid = @screenerid
                AND screenerresults.date >= @from
            GROUP BY {grouping}
            ORDER BY count DESC"

        let results =
            cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@from", Sql.timestamp (DateTime.Now.Date.AddDays(-days));
                "@screenerid", Sql.int screener.id
            ]
            |> Sql.execute (fun reader ->
                (reader.string grouping, reader.int "count")
            )

        results
            |> Seq.toList

    let topSectors screener days =
        "sector" |> topGrouping screener days

    let topIndustries screener days =
        "industry" |> topGrouping screener days

    let topCountries screener days =
        "industry" |> topGrouping screener days

    let getLatestScreeners() =
        let sql = @$"SELECT screeners.id,name,url,count(*) as count FROM screenerresults
            JOIN screeners ON screenerresults.screenerid = screeners.id
            WHERE date = (SELECT MAX(date) FROM screenerresults)
            GROUP BY screeners.id,name,url
            ORDER BY screeners.id"

        let results =
            cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.execute (fun reader ->
                (
                    {
                        id = (reader.int "id");
                        name = (reader.string "name");
                        url = (reader.string "url");
                    },
                    reader.int "count"
                )
            )

        results
            |> Seq.toList

    let getScreenerResults id date =

        let sql = @$"SELECT stocks.id,ticker,name,sector,industry,country,screenerresults.price,screenerresults.change,screenerresults.volume FROM stocks
            JOIN screenerresults ON stocks.id = screenerresults.stockid
            WHERE 
                screenerresults.screenerid = @screenerid
                AND screenerresults.date = date(@date)
            ORDER BY screenerresults.volume DESC"

        let results =
            cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@date", Sql.string date;
                "@screenerid", Sql.int id
            ]
            |> Sql.execute (fun reader -> 
                {
                    stockid = (reader.int "id");
                    ticker = (reader.string "ticker");
                    name = (reader.string "name");
                    sector = (reader.string "sector");
                    industry = (reader.string "industry");
                    country = (reader.string "country");
                    price = (reader.decimal "price");
                    change = (reader.decimal "change");
                    volume = (reader.int "volume");
                })

        results
        