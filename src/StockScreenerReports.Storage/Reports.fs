namespace StockScreenerReports.Storage

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

    let private industrySMABreakdownMapper (reader:RowReader) : StockScreenerReports.Core.IndustrySMABreakdown =
        {
            industry = reader.string "industry";
            breakdown = (
                {
                    date = (reader.dateTime "date");
                    days = (reader.int "days");
                    above = (reader.int "above");
                    below = (reader.int "below");
                }
            );
        }

    let private industryTrendMapper (reader:RowReader) : StockScreenerReports.Core.IndustryTrend =
        {
            industry = reader.string "industry";
            streak = reader.int "streak";
            direction = (
                match reader.string "direction" with
                    | "up" -> StockScreenerReports.Core.Up
                    | "down" -> StockScreenerReports.Core.Down
                    | _ -> StockScreenerReports.Core.Up
            );
            days = reader.int "days";
            date = reader.dateTime "date";
            change = reader.decimal "change";
        }

    let private smaBreakdownMapper (reader:RowReader) : StockScreenerReports.Core.SMABreakdown =
        {
            date = (reader.dateTime "date");
            days = (reader.int "days");
            above = (reader.int "above");
            below = (reader.int "below");
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

    let getScreenerResultsForDays id days =

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
                AND screenerresults.date >= current_date - @days
            ORDER BY screenerresults.date DESC, ticker DESC"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@screenerid", Sql.int id;
                "@days", Sql.int days
            ]
            |> Sql.execute (fun reader ->
                mapScreenerResultReportItem reader
            )

    let getTickersWithEarnings date =
        let sql = @$"SELECT ticker FROM earnings
            WHERE date = date(@date)"
        let results =
            cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@date", Sql.string date
            ]
            |> Sql.execute (fun reader -> reader.string "ticker")
        results

    let getAllEarningsTickers() =
        let sql = @$"SELECT ticker,date FROM earnings"
        
        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.execute (fun reader -> (reader.string "ticker", reader.dateTime "date"))

    let getEarningsTickers (startDate:DateTimeOffset) (endDate:DateTimeOffset) =
        let sql = @$"SELECT ticker,date FROM earnings
            WHERE date >= @start AND date <= @end
            ORDER BY date ASC, ticker ASC"
        
        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@start", Sql.timestamptz startDate;
            "@end", Sql.timestamptz endDate
        ]
        |> Sql.execute (fun reader -> (reader.string "ticker", reader.dateTime "date"))
    
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

    let getAllScreenerResults id =

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
            ORDER BY screenerresults.id"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@screenerid", Sql.int id
            ]
            |> Sql.execute (fun reader -> mapScreenerResultReportItem reader)

    let getDailyTotalVolumeForScreener id days =

        let sql = @$"
            SELECT 
                date,sum(volume) as totalvolume
            FROM screenerresults
            WHERE 
                screenerid = @screenerid
                AND date >= current_date - @days
            GROUP BY date
            ORDER BY date DESC"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@screenerid", Sql.int id;
                "@days", Sql.int days
            ]
            |> Sql.execute (fun reader ->
                (reader.dateTime "date", reader.int "totalvolume")
            )

    let getDailyAverageVolumeForScreener id days =

        let sql = @$"
            SELECT 
                date,avg(volume) as count
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
                "@days", Sql.int days;
                "@screenerid", Sql.int id
            ]
            |> Sql.execute (fun reader ->
                (reader.dateTime "date", reader.int "count")
            )


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

    let private getDailyCountsForScreenerAndStockFilter id filterColumn filterValue days =
        let sql = @$"
            SELECT 
                date,count(*) as count
            FROM screenerresults
            JOIN stocks ON stocks.id = screenerresults.stockid
            WHERE 
                screenerid = @screenerid
                AND date >= current_date - @days
                AND " + filterColumn + " = @" + filterColumn + "
            GROUP BY date
            ORDER BY date"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@screenerid", Sql.int id;
                "@days", Sql.int days;
                $"@{filterColumn}", Sql.string filterValue
            ]
            |> Sql.execute (fun reader -> 
                (
                    reader.dateTime "date",
                    reader.int "count"
                )
            )

    let getDailyCountsForScreenerAndSector id sector days =
        getDailyCountsForScreenerAndStockFilter id "sector" sector days

    let getDailyCountsForScreenerAndIndustry id industry days =
        getDailyCountsForScreenerAndStockFilter id "industry" industry days

    let getDailyCountsForScreenerAndCountry id country days =
        getDailyCountsForScreenerAndStockFilter id "country" country days

    let getScreenerResultsForTickerDayRange (ticker:StockScreenerReports.Core.StockTicker.T) days =
            
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
                AND screenerresults.date >= current_date - @days
            ORDER BY screenerresults.date DESC"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@ticker", ticker |> StockScreenerReports.Core.StockTicker.value |> Sql.string
                "@days", Sql.int days
            ]
            |> Sql.execute mapScreenerResultReportItem

    let getScreenerResultsForTicker (ticker:StockScreenerReports.Core.StockTicker.T) limit =
            
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
            ORDER BY screenerresults.date DESC
            LIMIT @limit"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@ticker", ticker |> StockScreenerReports.Core.StockTicker.value |> Sql.string
                "@limit", Sql.int limit
            ]
            |> Sql.execute mapScreenerResultReportItem

    let getScreenerResultsForSector limit sector =

        let sql = @$"
            SELECT 
                stocks.id,ticker,stocks.name,sector,industry,country,
                screeners.id as screenerid,screeners.name as screenername,
                screenerresults.date,marketcap,price,change,volume
            FROM stocks
            JOIN screenerresults ON stocks.id = screenerresults.stockid
            JOIN screeners ON screeners.id = screenerresults.screenerid
            WHERE 
                sector = @sector
            ORDER BY screenerresults.date DESC
            LIMIT @limit"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@sector", Sql.string sector;
                "@limit", Sql.int limit
            ]
            |> Sql.execute mapScreenerResultReportItem

    let getScreenerResultsForIndustry limit industry =
                
            let sql = @$"
                SELECT 
                    stocks.id,ticker,stocks.name,sector,industry,country,
                    screeners.id as screenerid,screeners.name as screenername,
                    screenerresults.date,marketcap,price,change,volume
                FROM stocks
                JOIN screenerresults ON stocks.id = screenerresults.stockid
                JOIN screeners ON screeners.id = screenerresults.screenerid
                WHERE 
                    industry = @industry
                ORDER BY screenerresults.date DESC
                LIMIT @limit"
    
            cnnString
                |> Sql.connect
                |> Sql.query sql
                |> Sql.parameters [
                    "@industry", Sql.string industry
                    "@limit", Sql.int limit
                ]
                |> Sql.execute mapScreenerResultReportItem


    let getScreenerResultsForCountry limit country =

        let sql = @$"
            SELECT 
                stocks.id,ticker,stocks.name,sector,industry,country,
                screeners.id as screenerid,screeners.name as screenername,
                screenerresults.date,marketcap,price,change,volume
            FROM stocks
            JOIN screenerresults ON stocks.id = screenerresults.stockid
            JOIN screeners ON screeners.id = screenerresults.screenerid
            WHERE 
                country = @country
            ORDER BY screenerresults.date DESC
            LIMIT @limit"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@country", Sql.string country;
                "@limit", Sql.int limit
            ]
            |> Sql.execute mapScreenerResultReportItem

    let getTopIndustriesForScreeners days screenerIds =

        let sql = @$"
            SELECT 
                industry,count(DISTINCT stocks.ticker) as count
            FROM screenerresults
            JOIN stocks ON stocks.id = screenerresults.stockid
            WHERE 
                screenerresults.screenerid = ANY(@screenerid)
                AND screenerresults.date >= current_date - @days
            GROUP BY industry
            ORDER BY count DESC"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@screenerid", screenerIds |> List.toArray |> Sql.intArray;
                "@days", Sql.int days
            ]
            |> Sql.execute (fun reader -> 
                (
                    reader.string "industry",
                    reader.int "count"
                )
            )

    let getTopSectorsForScreener days screenerId =

        let sql = @$"
            SELECT 
                sector,count(DISTINCT stocks.ticker) as count
            FROM screenerresults
            JOIN stocks ON stocks.id = screenerresults.stockid
            WHERE 
                screenerresults.screenerid = @screenerid
                AND screenerresults.date >= current_date - @days
            GROUP BY sector
            ORDER BY count DESC"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@screenerid", Sql.int screenerId;
                "@days", Sql.int days
            ]
            |> Sql.execute (fun reader -> 
                (
                    reader.string "sector",
                    reader.int "count"
                )
            )

    let getStockByCountryBreakdown() =

        let sql = @$"
            SELECT 
                country,count(*) as count
            FROM stocks
            GROUP BY country
            ORDER BY count DESC"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.execute (fun reader -> 
                (
                    reader.string "country",
                    reader.int "count"
                )
            )

    let getDailySMABreakdown days limit = 
            
            let sql = @$"
               SELECT date,days,above,below
                FROM DailySMABreakdowns
                WHERE days = @days
                AND date >= current_date - @limit
                ORDER BY date"
    
            cnnString
                |> Sql.connect
                |> Sql.query sql
                |> Sql.parameters [
                    "@days", Sql.int days;
                    "@limit", Sql.int limit
                ]
                |> Sql.execute smaBreakdownMapper

    let getStockSMABreakdown days = 

        let sql = @$"
            SELECT 
                sum(above) as above,sum(below) as below
            FROM dailysmabreakdowns
            WHERE date = (select max(date) from dailysmabreakdowns where days = @days)
            AND days = @days"

        let result = 
            cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@days", Sql.int days
            ]
            |> Sql.execute (fun reader -> (reader.int "above", reader.int "below"))
            |> Storage.singleOrThrow "More than one SMA breakdown found"
        
        match result with
            | Some (above,below) -> (above,below)
            | None -> (0,0)

    let getIndustrySMABreakdownsForIndustry days dayOffset industry =
        let sql = @"
            SELECT industry,date,days,above,below
            FROM IndustrySMABreakdowns
            WHERE industry = @industry
            AND days = @days
            AND date >= current_date - @dayOffset
            ORDER BY date"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@industry", Sql.string industry;
            "@days", Sql.int days;
            "@dayOffset", Sql.int dayOffset
        ]
        |> Sql.execute industrySMABreakdownMapper

    let getIndustrySMABreakdowns days date =
        let sql = @"
            SELECT industry,date,days,above,below FROM IndustrySMABreakdowns
            WHERE date = date(@date)
            AND days = @days
            ORDER BY (above/(below + above)) DESC"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@date", Sql.string date;
            "@days", Sql.int days;
        ]
        |> Sql.execute industrySMABreakdownMapper

    let getMostRecentIndustrySMABreakdown days industry =
        let sql = @"
            SELECT industry,date,days,above,below FROM IndustrySMABreakdowns
            WHERE industry = @industry
            AND days = @days
            AND date = (SELECT MAX(date) FROM IndustrySMABreakdowns WHERE industry = @industry AND days = @days)"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@industry", Sql.string industry;
            "@days", Sql.int days;
        ]
        |> Sql.execute industrySMABreakdownMapper
        |> Storage.singleOrThrow "More than one industry trend for the same industry and days"

    let getIndustrySMABreakdownLatestDate() =
        cnnString
        |> Sql.connect
        |> Sql.query "SELECT MAX(date) as date FROM IndustrySMABreakdowns"
        |> Sql.executeRow (fun reader -> reader.dateTime "date")

    let getIndustryTrends days =
        let sql = @"
            SELECT industry,date,streak,direction,change,days FROM industrytrends
            WHERE days = @days
            ORDER BY industry"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@days", Sql.int days;
        ]
        |> Sql.execute industryTrendMapper

    let getIndustryTrend days industry =
        let sql = @"
            SELECT industry,date,streak,direction,change,days FROM industrytrends
            WHERE industry = @industry AND days = @days"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@industry", Sql.string industry;
            "@days", Sql.int days;
        ]
        |> Sql.execute industryTrendMapper
        |> Storage.singleOrThrow "More than one industry trend for the same industry and days"

    let getTickersForScreenerAndDates screenerId (fromDate:DateTimeOffset) (endDate:DateTimeOffset) =
        let sql = @"
            SELECT DISTINCT s.ticker FROM screenerresults r
            JOIN stocks s ON s.id = r.stockid
            WHERE screenerid = @screenerid
            AND date >= @fromDate
            AND date <= @endDate"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@screenerid", Sql.int screenerId;
            "@fromDate", Sql.timestamptz fromDate;
            "@endDate", Sql.timestamptz endDate;
        ]
        |> Sql.execute (fun reader -> reader.string "ticker")
    

    let getScreenerResultCombos screenerId1 screenerId2 =
        let sql = @"
            SELECT 
                ticker, date
            FROM screenerresults
            JOIN stocks ON stocks.id = screenerresults.stockid
            WHERE screenerid = @screenerid1
            INTERSECT
            SELECT 
                ticker, date
            FROM screenerresults
            JOIN stocks ON stocks.id = screenerresults.stockid
            WHERE screenerid = @screenerid2
            ORDER BY date,ticker"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@screenerid1", Sql.int screenerId1;
            "@screenerid2", Sql.int screenerId2;
        ]
        |> Sql.execute (fun reader -> (reader.string "ticker", reader.dateTime "date"))