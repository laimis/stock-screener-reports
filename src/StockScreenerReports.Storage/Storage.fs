namespace StockScreenerReports.Storage

module Storage =

    open Npgsql.FSharp
    open StockScreenerReports.Core

    let private defaultLogger (_:string) = ()

    // TODO: add logger
    let mutable private cnnString = ""
    let configureConnectionString str =
        cnnString <- str

    let mutable private logger = defaultLogger
    let configureLogger log =
        logger <- log

    let private stockSelect = "SELECT id, ticker, name, sector, industry, country, lastmarketcap, lastupdated FROM stocks"

    let stockMapper (reader:RowReader) =
        {
            id = reader.int "id";
            ticker = reader.string "ticker" |> StockTicker.create;
            company = reader.string "name";
            sector = reader.string "sector";
            industry = reader.string "industry";
            country = reader.string "country";
            marketCap = reader.decimalOrNone "lastmarketcap";
            lastUpdate = reader.dateTimeOrNone "lastupdated"
        }

    let industryCycleMapper (reader:RowReader) : IndustryWithCycle =
        
        let startPoint = {
            date = reader.dateTime "startdate";
            value = reader.decimal "startvalue"
        }
        let highPoint = {
            date = reader.dateTime "highdate";
            value = reader.decimal "highvalue"
        }
        let currentPoint = {
            date = reader.dateTime "currentdate";
            value = reader.decimal "currentvalue"
        }

        let cycle = {
            startPoint = startPoint;
            highPoint = highPoint;
            currentPoint = currentPoint;
        }

        let industry = reader.string "industry"

        (industry, cycle)

    let private toJobNameString jobName =
        match jobName with
            | ScreenerJob _ -> "screenerjob"
            | TrendsJob _ -> "industrytrendsjob"
            | TestJob _ -> "testjob"
            | EarningsJob _ -> "earningsjob"

    let private toJobName jobName =
        match jobName with
            | "screenerjob" -> ScreenerJob
            | "industrytrendsjob" -> TrendsJob
            | "testjob" -> TestJob
            | "earningsjob" -> EarningsJob
            | _ -> raise (new System.Exception($"Unknown job name: {jobName}"))
    let private toJobStatusString status =
        match status with
            | Success _ -> "success"
            | Failure _ -> "failure"

    let private toJobStatus status =
        match status with
            | "success" -> Success
            | "failure" -> Failure
            | _ -> raise (new System.Exception($"Unknown job status: {status}"))

    let jobMapper (reader:RowReader) : Job =

        {
            name = reader.string "name" |> toJobName;
            status = reader.string "status" |> toJobStatus;
            timestamp = reader.dateTime "timestamp";
            message = reader.string "message";
        }

    let private toTrendDirectionString (trendDirection:TrendDirection) =
        match trendDirection with
            | Up _ -> "up"
            | Down _ -> "down"

    let private toEarningTimeString earningsTime =
        match earningsTime with
            | BeforeMarket _ -> "beforemarket"
            | AfterMarket _ -> "aftermarket"

    let singleOrThrow message results =
        match results with
        | [] -> None
        | [x] -> Some x
        | _ -> raise (new System.Exception(message))

    let getStockByTickers (tickers:string list) =
        let sql = $"{stockSelect} WHERE ticker = ANY(@tickers)"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters ["@tickers", tickers |> Array.ofList |> Sql.stringArray]
            |> Sql.execute stockMapper
        
    let getStockByTicker (ticker:StockTicker.T) =
        let sql = $"{stockSelect} WHERE ticker = @ticker"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters ["@ticker", ticker |> StockTicker.value |> Sql.string]
            |> Sql.execute stockMapper
            |> singleOrThrow "Expected single result for stock"

    let findStocksByTicker (ticker:string) =
        let sql = $"{stockSelect} WHERE LOWER(ticker) LIKE LOWER(@ticker)"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters ["@ticker", $"%%{ticker}%%" |> Sql.string]
            |> Sql.execute stockMapper

    let findStocksByTickerOrName (query:string) =
        let sql = $"{stockSelect} WHERE LOWER(ticker) LIKE LOWER(@query) OR LOWER(name) LIKE LOWER(@query)"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters ["@query", $"%%{query}%%" |> Sql.string]
            |> Sql.execute stockMapper

    let updateStockTicker (oldTicker:StockTicker.T) (newTicker:StockTicker.T) =
        let sql = @"UPDATE stocks SET ticker = @newTicker WHERE ticker = @oldTicker"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@oldTicker", oldTicker |> StockTicker.value |> Sql.string;
                "@newTicker", newTicker |> StockTicker.value |> Sql.string
            ]
            |> Sql.executeNonQuery

    let getStocksBySector (sector:string) =
        let sql = $"{stockSelect} WHERE sector = @sector"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters ["@sector", sector |> Sql.string]
            |> Sql.execute stockMapper

    let getStocksByIndustry (industry:string) =
        let sql = $"{stockSelect} WHERE industry = @industry"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters ["@industry", industry |> Sql.string]
            |> Sql.execute stockMapper
    
    // TODO: should we consider types for sector, industry, country?
    
    let renameStockTicker (oldName:StockTicker.T) (newName:StockTicker.T) =
        let sql = @"UPDATE stocks SET ticker = @newTicker WHERE ticker = @oldTicker"
        
        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@oldTicker", oldName |> StockTicker.value |> Sql.string;
                "@newTicker", newName |> StockTicker.value |> Sql.string
            ]
            |> Sql.executeNonQuery
        
    let saveStock (ticker:StockTicker.T) name sector industry country marketCap =
        
        let sql = @"INSERT INTO stocks (ticker,name,sector,industry,country,lastmarketcap,lastupdated)
            VALUES (@ticker,@name,@sector,@industry,@country,@marketCap,now())
            ON CONFLICT (ticker)
            DO UPDATE
            SET sector=@sector, industry=@industry, country=@country, lastmarketcap=@marketCap, lastupdated=now() RETURNING *"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                    "@ticker", ticker |> StockTicker.value |> Sql.string;
                    "@name", Sql.string name;
                    "@sector", Sql.string sector;
                    "@industry", Sql.string industry;
                    "@country", Sql.string country;
                    "@marketCap", Sql.decimal marketCap
                ]
            |> Sql.executeRow stockMapper        

    let deleteStock (stock:Stock) =
        cnnString
        |> Sql.connect
        |> Sql.query "DELETE FROM stocks WHERE id = @stockId"
        |> Sql.parameters ["@stockId", Sql.int stock.id]
        |> Sql.executeNonQuery

    let private screenerMapper (reader:RowReader) =
        {
            id = reader.int "id";
            name = reader.string "name";
            url = reader.string "url";
        }

    let saveScreener name url =
        cnnString
            |> Sql.connect
            |> Sql.query "INSERT INTO screeners (name,url) VALUES (@name,@url) RETURNING *"
            |> Sql.parameters [
                "@name", Sql.string name;
                "@url", Sql.string url
            ]
            |> Sql.executeRow screenerMapper

    let getScreeners() =
        cnnString
        |> Sql.connect
        |> Sql.query "SELECT id,name,url FROM screeners ORDER BY id"
        |> Sql.execute screenerMapper
            
    let getScreenerByName name = 
        cnnString 
        |> Sql.connect
        |> Sql.query "SELECT id,name,url FROM screeners WHERE name = @name"
        |> Sql.parameters [ "@name", Sql.string name ]
        |> Sql.execute screenerMapper
        |> singleOrThrow "More than one screener with the same name"

    let getScreenerById id = 
        cnnString 
        |> Sql.connect
        |> Sql.query "SELECT id,name,url FROM screeners WHERE id = @id"
        |> Sql.parameters [ "@id", Sql.int id ]
        |> Sql.execute screenerMapper
        |> singleOrThrow "More than one screener with the same id"
    
    let deleteScreener screener =

        // start transaction
        cnnString
        |> Sql.connect
        |> Sql.executeTransaction [
            "DELETE FROM screenerresults WHERE screenerid = @id", [
                ["@id", Sql.int screener.id]
            ]
            "DELETE FROM screeners WHERE id = @id", [
                ["@id", Sql.int screener.id]
            ]
        ]

    let deleteScreenerResults (screener:Screener) (date:string) =
        
        let sql = @"DELETE FROM screenerresults
            WHERE
            screenerid = @screenerId
            AND date = date(@date)"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@screenerId", Sql.int screener.id;
            "@date", Sql.string date
        ]
        |> Sql.executeNonQuery

    let saveScreenerResult screener date (stock:Stock) (result:ScreenerResult) =

        let sql = @"INSERT INTO screenerresults
            (screenerid,date,stockId,marketcap,price,change,volume)
            VALUES
            (@screenerId,date(@date),@stockId,@marketcap,@price,@change,@volume)"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@screenerId", Sql.int screener.id;
            "@date", Sql.string date;
            "@stockId", Sql.int stock.id;
            "@marketcap", Sql.decimal result.marketCap;
            "@price", Sql.decimal result.price;
            "@change", Sql.decimal result.change;
            "@volume", Sql.int result.volume
        ]
        |> Sql.executeNonQuery

    let getOrSaveScreener (screener:Screener) =
        let screenerOption = getScreenerByName screener.name
        match screenerOption with
            | Some screener -> screener
            | None -> saveScreener screener.name screener.url

    let saveScreenerResults date (input:Screener,results:seq<ScreenerResult>) =
        
        let screener = getOrSaveScreener input
        
        deleteScreenerResults screener date |> ignore

        results
        |> Seq.map (fun (result:ScreenerResult) ->
            let stock = saveStock result.ticker result.company result.sector result.industry result.country result.marketCap
            (screener,stock,result)
        )
        |> Seq.iter (fun (screener,stock,result) ->
            saveScreenerResult screener date stock result |> ignore
        )
    
    let getIndustries() =
        cnnString
        |> Sql.connect
        |> Sql.query "SELECT DISTINCT industry FROM stocks ORDER BY industry"
        |> Sql.execute (fun reader -> reader.string "industry")

    let updateIndustryTrend (industrySmaBreakdown:IndustrySMABreakdown) (trend:Trend) =
        cnnString
        |> Sql.connect
        |> Sql.query @"INSERT INTO industrytrends (industry,date,above,below,streak,direction,change,days)
            VALUES (@industry,date(@date),@above,@below,@streak,@direction,@change,@days)
            ON CONFLICT (industry,days,date) DO UPDATE SET streak = @streak, direction = @direction, change = @change, days = @days"
        |> Sql.parameters [
            "@industry", industrySmaBreakdown.industry |> Sql.string;
            "@date", industrySmaBreakdown.breakdown.date |> Utils.convertToDateString |> Sql.string;
            "@above", Sql.int (industrySmaBreakdown.breakdown.above);
            "@below", Sql.int (industrySmaBreakdown.breakdown.below);
            "@streak", Sql.int (trend.streak);
            "@direction", trend.direction |> toTrendDirectionString |> Sql.string;
            "@days", industrySmaBreakdown.breakdown.days |> Sql.int;
            "@change", Sql.decimal (trend.change)
        ]
        |> Sql.executeNonQuery

    let updateSMABreakdowns date days =
        let sql = @"SELECT sum(above) as above,sum(below) as below,@days
            FROM industrysmabreakdowns
            WHERE date = date(@date) AND days = @days"

        let (above,below) = 
            cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@date", Sql.string date;
                "@days", Sql.int days
            ]
            |> Sql.executeRow (fun reader ->
                ((reader.intOrNone "above"), (reader.intOrNone "below"))
            )

        // check above and below are not null
        match above,below with
        | (Some above, Some below) -> 
            let sql = @"
                INSERT INTO dailysmabreakdowns (date,above,below,days)
                VALUES (date(@date),@above,@below,@days)
                ON CONFLICT (date,days) DO UPDATE SET above = @above, below = @below"

            cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@date", Sql.string date;
                "@above", Sql.int above;
                "@below", Sql.int below;
                "@days", Sql.int days
            ]
            |> Sql.executeNonQuery
        | _ -> 0

    let saveIndustryCycle days (cycle:MarketCycle) industry =

        let sql = @"INSERT INTO industrycycles (industry,days,startDate,startValue,highDate,highValue,currentDate,currentValue)
            VALUES (@industry,@days,date(@start),@startValue,date(@highDate),@highValue,date(@currentDate),@currentValue)
            ON CONFLICT (industry,days) DO UPDATE SET
                startDate = date(@start),
                startValue = @startValue,
                highDate = date(@highDate),
                highValue = @highValue,
                currentDate = date(@currentDate),
                currentValue = @currentValue"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@industry", Sql.string industry;
            "@days", Sql.int days;
            "@start", Sql.string cycle.startPointDateFormatted;
            "@startValue", Sql.decimal cycle.startPointValue;
            "@highDate", Sql.string cycle.highPointDateFormatted;
            "@highValue", Sql.decimal cycle.highPointValue;
            "@currentDate", Sql.string cycle.currentPointDateFormatted;
            "@currentValue", Sql.decimal cycle.currentPointValue;
        ]
        |> Sql.executeNonQuery

    let getIndustryCycle days industry =
        let sql = @"SELECT * FROM industrycycles WHERE industry = @industry AND days = @days"
        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@industry", Sql.string industry;
            "@days", Sql.int days;
        ]
        |> Sql.executeRow industryCycleMapper

    let getIndustryCycles days =
        let sql = @"SELECT * FROM industrycycles WHERE days = @days"
        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@days", Sql.int days;
        ]
        |> Sql.execute industryCycleMapper

    let saveIndustrySMABreakdowns date  (industry,days,above:int,below:int) =
        let sql = @"
            DELETE FROM IndustrySMABreakdowns WHERE industry = @industry AND date = date(@date) AND days = @days;

            INSERT INTO IndustrySMABreakdowns
            (industry,date,days,above,below)
            VALUES
            (@industry,date(@date),@days,@above,@below)"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@industry", Sql.string industry;
            "@date", Sql.string date;
            "@days", Sql.int days;
            "@above", Sql.int above;
            "@below", Sql.int below;
        ]
        |> Sql.executeNonQuery

    let saveEarningsDate ticker date earningsTime =
        let sql = @"INSERT INTO earnings (ticker,date,earningsTime) VALUES (@ticker,date(@date),@earningsTime)
            ON CONFLICT (ticker,date) DO NOTHING"
        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@ticker", ticker |> StockTicker.value |> Sql.string;
            "@date", Sql.string date;
            "@earningsTime", earningsTime |> toEarningTimeString |> Sql.string
        ]
        |> Sql.executeNonQuery

    let saveJobStatus (jobName:JobName) (timestamp : System.DateTime) (status:JobStatus) message =
        let sql = @"
            INSERT INTO jobs
            (name,timestamp,status,message)
            VALUES
            (@name,@timestamp,@status,@message)"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@name", jobName |> toJobNameString |> Sql.string;
            "@timestamp", Sql.timestamptz (timestamp.ToUniversalTime());
            "@status", status |> toJobStatusString |> Sql.string;
            "@message", Sql.string message;
        ]
        |> Sql.executeNonQuery

    let migrateDates fromDate toDate =
        
        let sql = @"
            update dailysmabreakdowns set date = date(@toDate) where date = date(@fromDate);
            update industrysmabreakdowns set date = date(@toDate) where date = date(@fromDate);
            update industrytrends set date = date(@toDate) where date = date(@fromDate);
            update screenerresults set date = date(@toDate) where date = date(@fromDate);"
        
        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@fromDate", Sql.string fromDate;
            "@toDate", Sql.string toDate;
        ]
        |> Sql.executeNonQuery

    let deleteDate date =
        let sql = @"
            delete from dailysmabreakdowns where date = date(@date);
            delete from industrysmabreakdowns where date = date(@date);
            delete from industrytrends where date = date(@date);
            delete from screenerresults where date = date(@date);"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@date", Sql.string date;
        ]
        |> Sql.executeNonQuery

    let getJobs() =
        let sql = @"
            WITH ranked_logs AS (
  SELECT
    name,
    status,
    message,
    timestamp,
    ROW_NUMBER() OVER (PARTITION BY name ORDER BY timestamp DESC) AS row_number
  FROM
    jobs
)
SELECT
    name,
    status,
    message,
    timestamp
FROM
  ranked_logs
WHERE
  row_number = 1;"
        
        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.execute jobMapper