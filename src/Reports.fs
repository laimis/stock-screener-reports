namespace FinvizScraper

module Reports =

    open Npgsql.FSharp
    open System

    let mutable private cnnString = ""

    let storeCnn str =
        cnnString <- str

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