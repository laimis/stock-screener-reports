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
            trend = (
                {
                    streak = reader.int "streak";
                    direction = (
                        match reader.string "direction" with
                            | "up" -> StockScreenerReports.Core.Up
                            | "down" -> StockScreenerReports.Core.Down
                            | x -> failwith $"invalid trend direction: {x}"
                    );
                    change = reader.decimal "change";
                    value = (reader.decimal "above") / (reader.decimal "below" + reader.decimal "above");
                }
            );
            above = reader.int "above";
            below = reader.int "below";
            days = reader.int "days";
            date = reader.dateTime "date";
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

    let private topGroupingOverDays screenerId dateRange grouping =
        let sql = @$"SELECT {grouping},count(*) as count FROM stocks
            JOIN screenerresults ON stocks.id = screenerresults.stockid
            WHERE 
                screenerresults.screenerid = @screenerId
                AND screenerresults.date BETWEEN DATE(@fromDate) AND DATE(@toDate)
            GROUP BY {grouping}
            ORDER BY count DESC"

        let results =
            cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@fromDate", dateRange |> fst |> Sql.string;
                "@toDate", dateRange |> snd |> Sql.string;
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

    let topSectorsOverDays scrennerId dateRange =
        "sector" |> topGroupingOverDays scrennerId dateRange

    let topIndustriesOverDays scrennerId dateRange =
        "industry" |> topGroupingOverDays scrennerId dateRange

    let topCountriesOverDays scrennerId dateRange =
        "country" |> topGroupingOverDays scrennerId dateRange

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

    let getScreenerResultsForDays dateRange id =

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
                AND screenerresults.date BETWEEN date(@startDate) AND date(@endDate)
            ORDER BY screenerresults.date DESC, ticker DESC"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@screenerid", Sql.int id;
                "@startDate", dateRange |> fst |> Sql.string
                "@endDate", dateRange |> snd |> Sql.string
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

    let getEarningsTickers dateRange =
        let sql = @$"SELECT ticker,date FROM earnings
            WHERE date >= date(@start) AND date <= date(@end)
            ORDER BY date ASC, ticker ASC"
        
        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@start",  dateRange |> fst |> Sql.string;
            "@end", dateRange |> snd |> Sql.string
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

    let getDailyTotalVolumeForScreener dateRange screenerId =

        let sql = @$"
            SELECT 
                date,sum(volume) as totalvolume
            FROM screenerresults
            WHERE 
                screenerid = @screenerid
                AND date BETWEEN date(@startDate) AND date(@endDate)
            GROUP BY date
            ORDER BY date DESC"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@screenerid", Sql.int screenerId;
                "@startDate", Sql.string (dateRange |> fst)
                "@endDate", Sql.string (dateRange |> snd)
            ]
            |> Sql.execute (fun reader ->
                (reader.dateTime "date", reader.int "totalvolume")
            )

    let getDailyAverageVolumeForScreener dateRange screenerId =

        let sql = @$"
            SELECT 
                date,avg(volume) as count
            FROM screenerresults
            WHERE 
                screenerid = @screenerid
                AND date BETWEEN date(@startDate) AND date(@endDate)
            GROUP BY date
            ORDER BY date"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@screenerid", Sql.int screenerId
                "@startDate", Sql.string (dateRange |> fst)
                "@endDate", Sql.string (dateRange |> snd)
            ]
            |> Sql.execute (fun reader ->
                (reader.dateTime "date", reader.int "count")
            )


    let getDailyCountsForScreener dateRange screernId =

        let sql = @$"
            SELECT 
                date,count(*) as count
            FROM screenerresults
            WHERE 
                screenerid = @screenerid
                AND date BETWEEN date(@startDate) AND date(@endDate)
            GROUP BY date
            ORDER BY date"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@screenerid", Sql.int screernId
                "@startDate", Sql.string (dateRange |> fst)
                "@endDate", Sql.string (dateRange |> snd)
            ]
            |> Sql.execute (fun reader -> 
                (
                    reader.dateTime "date",
                    reader.int "count"
                )
            )

    let private getDailyCountsForScreenerAndStockFilter id filterColumn filterValue dateRange =
        let sql = @$"
            SELECT 
                date,count(*) as count
            FROM screenerresults
            JOIN stocks ON stocks.id = screenerresults.stockid
            WHERE 
                screenerid = @screenerid
                AND date BETWEEN date(@startDate) AND date(@endDate)
                AND " + filterColumn + " = @" + filterColumn + "
            GROUP BY date
            ORDER BY date"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@screenerid", Sql.int id;
                "@startDate", (dateRange |> fst |> Sql.string);
                "@endDate", (dateRange |> snd |> Sql.string);
                $"@{filterColumn}", Sql.string filterValue
            ]
            |> Sql.execute (fun reader -> 
                (
                    reader.dateTime "date",
                    reader.int "count"
                )
            )

    let getDailyCountsForScreenerAndSector id sector =
        getDailyCountsForScreenerAndStockFilter id "sector" sector

    let getDailyCountsForScreenerAndIndustry id industry =
        getDailyCountsForScreenerAndStockFilter id "industry" industry

    let getDailyCountsForScreenerAndCountry id country =
        getDailyCountsForScreenerAndStockFilter id "country" country

    let getScreenerResultsForTickerDayRange (ticker:StockScreenerReports.Core.StockTicker.T) dateRange =
            
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
                AND screenerresults.date BETWEEN date(@startDate) AND date(@endDate)
            ORDER BY screenerresults.date DESC"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@ticker", ticker |> StockScreenerReports.Core.StockTicker.value |> Sql.string
                "@startDate", Sql.string (dateRange |> fst)
                "@endDate", Sql.string (dateRange |> snd)
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

    let getScreenerResultsForIndustry dateRange limit industry =
                
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
                    AND screenerresults.date BETWEEN date(@startDate) AND date(@endDate)
                ORDER BY screenerresults.date DESC
                LIMIT @limit"
    
            cnnString
                |> Sql.connect
                |> Sql.query sql
                |> Sql.parameters [
                    "@industry", Sql.string industry
                    "@limit", Sql.int limit
                    "@startDate", Sql.string (dateRange |> fst)
                    "@endDate", Sql.string (dateRange |> snd)
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

    let getDailySMABreakdown dateRange days = 
            
            let sql = @$"
               SELECT date,days,above,below
                FROM DailySMABreakdowns
                WHERE days = @days
                AND date BETWEEN date(@startDate) AND date(@endDate)
                ORDER BY date"
    
            cnnString
                |> Sql.connect
                |> Sql.query sql
                |> Sql.parameters [
                    "@days", Sql.int days;
                    "@startDate", dateRange |> fst |> Sql.string;
                    "@endDate", dateRange |> snd |> Sql.string;
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

    let getIndustrySMABreakdownsForIndustryAndDateRange days startDate endDate industry =
        let sql = @"
            SELECT industry,date,days,above,below
            FROM IndustrySMABreakdowns
            WHERE industry = @industry
            AND days = @days
            AND date >= date(@startDate)
            AND date <= date(@endDate)
            ORDER BY date"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@industry", Sql.string industry;
            "@days", Sql.int days;
            "@startDate", Sql.string startDate;
            "@endDate", Sql.string endDate;
        ]
        |> Sql.execute industrySMABreakdownMapper
        
    let getIndustrySMABreakdownsForIndustry days dateRange industry =
        let sql = @"
            SELECT industry,date,days,above,below
            FROM IndustrySMABreakdowns
            WHERE industry = @industry
            AND days = @days
            AND date BETWEEN date(@startDate) AND date(@endDate)
            ORDER BY date"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@industry", Sql.string industry;
            "@days", Sql.int days;
            "@startDate", dateRange |> fst |> Sql.string;
            "@endDate", dateRange |> snd |> Sql.string;
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
        |> Storage.singleOrThrow "More than one industry sma breakdown for the same industry and days"

    let getIndustrySMABreakdownLatestDate() =
        cnnString
        |> Sql.connect
        |> Sql.query "SELECT MAX(date) as date FROM IndustrySMABreakdowns"
        |> Sql.executeRow (fun reader -> reader.dateTime "date")

    let getIndustryTrends date days =
        let sql = @"
            SELECT industry,date,above,below,streak,direction,change,days FROM industrytrends
            WHERE 
                date = date(@date)
                AND days = @days
            ORDER BY industry"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@date", Sql.string date;
            "@days", Sql.int days;
        ]
        |> Sql.execute industryTrendMapper

    let getTopIndutriesTrending date numberOfRecords (direction:StockScreenerReports.Core.TrendDirection)  =
        
        let descOrAsc = 
            match direction with
            | StockScreenerReports.Core.TrendDirection.Up -> "DESC"
            | StockScreenerReports.Core.TrendDirection.Down -> "ASC"

        let changeCriteria =
            match direction with
            | StockScreenerReports.Core.TrendDirection.Up -> "change > 0"
            | StockScreenerReports.Core.TrendDirection.Down -> "change < 0"

        let sql = @"
            SELECT industry,date,above,below,streak,direction,change,days FROM industrytrends
            WHERE 
                date = date(@date)
                AND days = 20
                AND " + changeCriteria + @"
            ORDER BY change/streak " + descOrAsc + @"
            LIMIT @numberOfRecords"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@numberOfRecords", Sql.int numberOfRecords;
            "@date", date |> Sql.string;
        ]
        |> Sql.execute industryTrendMapper

    let getIndustryTrendsLastKnownDateAsOf date =
        let sql = @"
            SELECT MAX(date) as date FROM industrytrends
            WHERE date <= date(@date)"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@date", Sql.string date;
        ]
        |> Sql.executeRow (fun reader -> reader.dateTimeOrNone "date")

    let getIndustryTrendBreakdown date days =
        let sql = @"
            SELECT
                COALESCE(SUM(case WHEN direction = 'up' THEN 1 ELSE 0 END),0) up,
                COALESCE(SUM(case WHEN direction = 'down' THEN 1 ELSE 0 END),0) down
            FROM industrytrends
            WHERE 
                date = date(@date)
                AND days = @days"

        try
            cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@days", Sql.int days;
                "@date", Sql.string date;
            ]
            |> Sql.executeRow (fun reader -> (reader.int "up", reader.int "down"))
        with
            | ex -> 
                System.Console.WriteLine("Error: " + date + " " + days.ToString())
                reraise()

    let getIndustryTrend days date industry =
        let sql = @"
            SELECT industry,date,above,below,streak,direction,change,days FROM industrytrends
            WHERE industry = @industry AND days = @days
            AND date = date(@date)"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@industry", Sql.string industry;
            "@days", Sql.int days;
            "@date", Sql.string date;
        ]
        |> Sql.execute industryTrendMapper
        |> Storage.singleOrThrow "More than one industry trend for the same industry and days"

    let getStocksForScreenerAndDates (fromDate:DateTimeOffset) (endDate:DateTimeOffset) screenerId =
        let sql = @"
            SELECT DISTINCT s.* FROM screenerresults r
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
        |> Sql.execute Storage.stockMapper
    

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