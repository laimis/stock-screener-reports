namespace FinvizScraper.Storage

module Reports =

    open Npgsql.FSharp
    open System
    open FinvizScraper.Core

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
            marketCap:decimal;
            price:decimal;
            change:decimal;
            volume:int;
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
            |> Seq.toList

    let topSectors screenerId date =
        "sector" |> topGrouping screenerId date

    let topIndustries screenerId date =
        "industry" |> topGrouping screenerId date

    let topCountries screenerId date =
        "country" |> topGrouping screenerId date

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
                stocks.id,ticker,name,sector,industry,country,
                screenerresults.marketcap,price,change,volume
            FROM stocks
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
                    marketCap = (
                        reader.decimalOrNone "marketcap"
                        |> Option.defaultValue 0m
                    );
                    price = (reader.decimal "price");
                    change = (reader.decimal "change");
                    volume = (reader.int "volume");
                })

        results
        