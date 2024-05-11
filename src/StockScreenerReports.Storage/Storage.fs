namespace StockScreenerReports.Storage

module Storage =

    open Npgsql.FSharp
    open StockScreenerReports.Core

    let private defaultLogger (_:string) = ()

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
            | ScreenerJob -> "screenerjob"
            | TrendsJob -> "industrytrendsjob"
            | TestJob -> "testjob"
            | EarningsJob -> "earningsjob"
            | CountriesJob -> "countriesjob"
            | AlertsJob -> "alertsjob"
            | CorporateActionsJob -> "corporateactionsjob"

    let private toJobName jobName =
        match jobName with
            | "screenerjob" -> ScreenerJob
            | "industrytrendsjob" -> TrendsJob
            | "testjob" -> TestJob
            | "earningsjob" -> EarningsJob
            | "countriesjob" -> CountriesJob
            | "alertsjob" -> AlertsJob
            | "corporateactionsjob" -> CorporateActionsJob
            | _ -> raise (System.Exception($"Unknown job name: {jobName}"))
    let private toJobStatusString status =
        match status with
            | Success -> "success"
            | Warning -> "warning"
            | Failure -> "failure"

    let private toJobStatus status =
        match status with
            | "success" -> Success
            | "failure" -> Failure
            | "warning" -> Warning
            | _ -> raise (System.Exception($"Unknown job status: {status}"))

    let jobMapper (reader:RowReader) : Job =

        {
            name = reader.string "name" |> toJobName;
            status = reader.string "status" |> toJobStatus;
            timestamp = reader.dateTime "timestamp";
            message = reader.string "message";
        }

    let private toTrendDirectionString (trendDirection:TrendDirection) =
        match trendDirection with
            | Up -> "up"
            | Down -> "down"

    let private toEarningTimeString earningsTime =
        match earningsTime with
            | BeforeMarket -> "beforemarket"
            | AfterMarket -> "aftermarket"

    let singleOrThrow message results =
        match results with
        | [] -> None
        | [x] -> Some x
        | _ -> raise (System.Exception(message))

    let getStockByTickers (tickers:string seq) =
        let sql = $"{stockSelect} WHERE ticker = ANY(@tickers)"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters ["@tickers", tickers |> Seq.toArray |> Sql.stringArray]
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
    
    let getStocksByCountry country =
        let sql = $"{stockSelect} WHERE country = @country"
        
        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters ["@country", country |> Sql.string]
            |> Sql.execute stockMapper
    
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
            
    let changeStockIndustry (ticker:StockTicker.T) (newIndustry:string) =
        let sql = @"UPDATE stocks SET industry = @industry WHERE ticker = @ticker"
        
        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@ticker", ticker |> StockTicker.value |> Sql.string;
                "@industry", newIndustry |> Sql.string
            ]
            |> Sql.executeNonQuery
        
    let saveStock (ticker:StockTicker.T) name sector industry country marketCap =
        
        let sql = @"INSERT INTO stocks (ticker,name,sector,industry,country,lastmarketcap,lastupdated)
            VALUES (@ticker,@name,@sector,@industry,@country,@marketCap,now())
            ON CONFLICT (ticker)
            DO UPDATE
            SET sector = @sector, industry = @industry, country = @country, lastmarketcap = @marketCap, lastupdated=now() RETURNING *"

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
        |> Sql.executeTransaction [
            
            "DELETE FROM screenerresults WHERE stockId = @stockId", [
                ["@stockId", Sql.int stock.id]
            ]
            
            "DELETE FROM stocks WHERE id = @stockId", [
                ["@stockId", Sql.int stock.id]
            ]
        ]

    let private screenerMapper (reader:RowReader) =
        {
            id = reader.int "id";
            name = reader.string "name";
            url = reader.string "url";
        }

    let createScreener name url =
        cnnString
            |> Sql.connect
            |> Sql.query "INSERT INTO screeners (name,url) VALUES (@name,@url) RETURNING *"
            |> Sql.parameters [
                "@name", Sql.string name;
                "@url", Sql.string url
            ]
            |> Sql.executeRow screenerMapper
            
    let updateScreener id name url =
        cnnString
        |> Sql.connect
        |> Sql.query "UPDATE screeners SET name = @name, url = @url WHERE id = @id RETURNING *"
        |> Sql.parameters [
            "@name", Sql.string name;
            "@url", Sql.string url;
            "@id", Sql.int id
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
            "@volume", Sql.int64 result.volume
        ]
        |> Sql.executeNonQuery

    let getOrSaveScreener (screener:Screener) =
        let screenerOption = getScreenerByName screener.name
        match screenerOption with
            | Some screener -> screener
            | None -> createScreener screener.name screener.url

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
        
    let getCountries() =
        cnnString
        |> Sql.connect
        |> Sql.query "SELECT DISTINCT country FROM stocks ORDER BY country"
        |> Sql.execute (fun reader -> reader.string "country")
        
    let updateIndustryTrend (industrySmaBreakdown:IndustrySMABreakdown) (trend:Trend) =
        cnnString
        |> Sql.connect
        |> Sql.query @"INSERT INTO industrytrends (industry,date,above,below,streak,direction,change,days)
            VALUES (@industry,date(@date),@above,@below,@streak,@direction,@change,@days)
            ON CONFLICT (industry,days,date) DO UPDATE SET streak = @streak, direction = @direction, change = @change, days = @days"
        |> Sql.parameters [
            "@industry", industrySmaBreakdown.industry |> Sql.string;
            "@date", industrySmaBreakdown.breakdown.date |> Utils.convertToDateString |> Sql.string;
            "@above", Sql.int industrySmaBreakdown.breakdown.above;
            "@below", Sql.int industrySmaBreakdown.breakdown.below;
            "@streak", Sql.int trend.streak;
            "@direction", trend.direction |> toTrendDirectionString |> Sql.string;
            "@days", industrySmaBreakdown.breakdown.days.Interval |> Sql.int;
            "@change", Sql.decimal trend.change
        ]
        |> Sql.executeNonQuery

    let updateIndustrySMABreakdowns date (sma:SMA) =
        let sql = @"SELECT sum(above) as above,sum(below) as below,@days
            FROM industrysmabreakdowns
            WHERE date = date(@date) AND days = @days"

        let above,below = 
            cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                "@date", Sql.string date;
                "@days", sma.Interval |> Sql.int
            ]
            |> Sql.executeRow (fun reader ->
                ((reader.intOrNone "above"), (reader.intOrNone "below"))
            )

        // check above and below are not null
        match above,below with
        | Some above, Some below -> 
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
                "@days", sma.Interval |> Sql.int 
            ]
            |> Sql.executeNonQuery
        | _ -> 0

    let saveIndustryCycle (sma:SMA) (cycle:MarketCycle) industry =

        let sql = @"INSERT INTO industrycycles (industry,days,startDate,startValue,highDate,highValue,currentDate,currentValue)
            VALUES (@industry,@days,date(@start),@startValue,date(@highDate),@highValue,date(@currentDate),@currentValue)
            ON CONFLICT (industry,days,currentDate) DO UPDATE SET
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
            "@days", sma.Interval |> Sql.int;
            "@start", Sql.string cycle.startPointDateFormatted;
            "@startValue", Sql.decimal cycle.startPointValue;
            "@highDate", Sql.string cycle.highPointDateFormatted;
            "@highValue", Sql.decimal cycle.highPointValue;
            "@currentDate", Sql.string cycle.currentPointDateFormatted;
            "@currentValue", Sql.decimal cycle.currentPointValue;
        ]
        |> Sql.executeNonQuery

    let getIndustryCycle (sma:SMA) industry =
        let sql = @"SELECT * FROM industrycycles WHERE industry = @industry AND days = @days AND currentDate = (SELECT MAX(currentDate) FROM industrycycles WHERE industry = @industry AND days = @days)"
        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@industry", Sql.string industry;
            "@days", sma.Interval |> Sql.int;
        ]
        |> Sql.executeRow industryCycleMapper

    let getIndustryCycleLatestDate (sma:SMA) date =
        let sql = @"SELECT MAX(currentDate) as max FROM industrycycles WHERE days = @days AND currentDate <= date(@date)"
        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@days", sma.Interval |> Sql.int;
            "@date", date |> Sql.string
        ]
        |> Sql.executeRow (fun reader -> reader.dateTime "max")
        
    let getIndustryCyclesForDate (sma:SMA) date =
        let sql = @"SELECT * FROM industrycycles WHERE days = @days AND currentDate = date(@date)"
        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@days", sma.Interval |> Sql.int;
            "@date", date |> Sql.string
        ]
        |> Sql.execute industryCycleMapper
        
    let getIndustryCycles (sma:SMA) =
        let sql = @"SELECT * FROM industrycycles WHERE days = @days AND currentDate = (SELECT MAX(currentDate) FROM industrycycles WHERE days = @days)"
        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@days", sma.Interval |> Sql.int;
        ]
        |> Sql.execute industryCycleMapper

    let saveIndustrySMABreakdowns date  (industry,sma:SMA,above:int,below:int) =
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
            "@days", sma.Interval |> Sql.int;
            "@above", Sql.int above;
            "@below", Sql.int below;
        ]
        |> Sql.executeNonQuery
        
    let saveCountrySMABreakdowns date  (country,sma:SMA,above:int,below:int) =
        let sql = @"
            DELETE FROM CountrySMABreakdowns WHERE country = @country AND date = date(@date) AND days = @days;

            INSERT INTO CountrySMABreakdowns
            (country,date,days,above,below)
            VALUES
            (@country,date(@date),@days,@above,@below)"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@country", Sql.string country;
            "@date", Sql.string date;
            "@days", sma.Interval |> Sql.int;
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
    
    let private sequenceTypeToString (t:IndustrySequenceType) =
        match t with
        | High -> "High"
        | Low -> "Low"
        
    let private sequenceTypeFromString (s:string) =
        match s with
        | nameof(High) -> High
        | nameof(Low) -> Low
        | _ -> failwith $"Invalid IndustrySequenceType {s}"
        
    let saveIndustrySequenceWithPoints (sequence: IndustrySequence) =
        let upsertSequenceSql = @"INSERT INTO industrysequences (industry, sequencetype, startdate, enddate, open)
                                  VALUES (@industry, @sequencetype, @startdate, @enddate, @open)
                                  ON CONFLICT (industry, sequencetype, startdate) DO UPDATE
                                  SET enddate = @enddate, open = @open
                                  RETURNING id"

        let deletePointsSql = @"DELETE FROM industrysequencepoints WHERE sequenceid = @sequenceid"

        let insertPointSql = @"INSERT INTO industrysequencepoints (sequenceid, date, value)
                               VALUES (@sequenceid, @date, @value)"

        let sequenceId =
            cnnString
            |> Sql.connect
            |> Sql.query upsertSequenceSql
            |> Sql.parameters [
                "@industry", Sql.string sequence.industry
                "@sequencetype", sequence.type' |> sequenceTypeToString |> Sql.string
                "@startdate", Sql.timestamp sequence.start.date;
                "@enddate", Sql.timestamp sequence.end'.date;
                "@open", Sql.bool sequence.open'
            ]
            |> Sql.executeRow (fun reader -> reader.int64 "id")
            
        cnnString
        |> Sql.connect
        |> Sql.query deletePointsSql
        |> Sql.parameters [
            "@sequenceid", Sql.int64 sequenceId
        ]
        |> Sql.executeNonQuery |> ignore
        
        sequence.values
        |> Seq.iter (fun pt ->
            cnnString
            |> Sql.connect
            |> Sql.query insertPointSql
            |> Sql.parameters [
                "@sequenceid", Sql.int64 sequenceId;
                "@date", Sql.timestamp pt.date;
                "@value", Sql.decimal pt.value
            ]
            |> Sql.executeNonQuery |> ignore
        )
    
    let private getIndustrySequencesWithQuery querySql queryParams =
        
        let sequenceMapper (reader:RowReader) =
            (
                reader.int64 "id",
                reader.string "industry",
                reader.string "sequencetype",
                reader.bool "open"
            )

        let pointMapper (reader:RowReader) =
            {
                date = reader.dateTime "date";
                value = reader.decimal "value"
            }

        let sequences =
            cnnString
            |> Sql.connect
            |> Sql.query querySql
            |> Sql.parameters queryParams
            |> Sql.execute sequenceMapper

        let pointSql = "SELECT * FROM industrysequencepoints WHERE sequenceid = @sequenceid ORDER BY id"

        sequences
        |> List.map (fun (sequenceId, industry, sequenceType, open') ->
            let points =
                cnnString
                |> Sql.connect
                |> Sql.query pointSql
                |> Sql.parameters ["@sequenceid", Sql.int64 sequenceId]
                |> Sql.execute pointMapper

            {
                industry = industry
                type' = sequenceType |> sequenceTypeFromString
                values = points
                open' = open' 
            }
        )
        
    let getIndustrySequencesForIndustry industry =
        let sequenceSql = "SELECT * FROM industrysequences WHERE industry = @industry ORDER BY startdate DESC"
        let sequenceParams = ["@industry", Sql.string industry]
        
        getIndustrySequencesWithQuery sequenceSql sequenceParams
        
    let getActiveIndustrySequences () =
        let sequenceSql = "SELECT * FROM industrysequences WHERE open = true ORDER BY startdate"
        let sequenceParams = []
        
        getIndustrySequencesWithQuery sequenceSql sequenceParams
        
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
        
       
    let private toSentimentString sentiment =
        match sentiment with
        | Positive -> "Positive"
        | Negative -> "Negative"
        | Neutral -> "Neutral"

    let private toSentiment str =
        match str with
        | "Positive" -> Positive
        | "Negative" -> Negative
        | "Neutral" -> Neutral
        | _ -> failwith $"Unknown sentiment: {str}"
        
    let saveAlert (alert:Alert) =
        let alertSql = @"INSERT INTO alerts (identifier, alerttype, industry, screenerid, date, sentiment, description, strength, ticker, corporateactiontype)
                     VALUES (@identifier, @alerttype, @industry, @screenerid, @date, @sentiment::sentiment, @description, @strength, @ticker, @corporateactiontype)
                     ON CONFLICT (identifier) DO UPDATE
                     SET alerttype = @alerttype, industry = @industry, screenerid = @screenerid,
                         date = @date, sentiment = @sentiment::sentiment, description = @description, strength = @strength,
                         ticker = @ticker, corporateactiontype = @corporateactiontype"

        let acknowledgementSql = @"INSERT INTO alert_acknowledgements (alert_identifier, acknowledged)
                               VALUES (@identifier, @acknowledged)
                               ON CONFLICT (alert_identifier) DO UPDATE SET acknowledged = @acknowledged"
                               
        let industry =
            match alert.alertType with
            | IndustryAlert industry -> industry |> Sql.string
            | IndustryScreenerAlert (industry,_) -> industry |> Sql.string
            | _ -> Sql.dbnull
            
        let screenerId =
            match alert.alertType with
            | IndustryScreenerAlert (_,screenerId) -> screenerId |> Sql.int
            | _ -> Sql.dbnull
            
        let alertType =
            match alert.alertType with
            | IndustryAlert _ -> nameof(IndustryAlert)
            | ScreenerAlert _ -> nameof(ScreenerAlert)
            | IndustryScreenerAlert _ -> nameof(IndustryScreenerAlert)
            | CorporateActionAlert _ -> nameof(CorporateActionAlert)
            
        let ticker =
            match alert.alertType with
            | CorporateActionAlert (ticker,_) -> ticker |> Sql.string
            | _ -> Sql.dbnull
            
        let corporateActionType =
            match alert.alertType with
            | CorporateActionAlert (_,actionType) -> actionType |> Sql.string
            | _ -> Sql.dbnull
            
        let parameters = [
            "@industry", industry
            "@screenerid", screenerId
            "@identifier", alert.identifier |> Sql.string
            "@alerttype", alertType |> Sql.string
            "@sentiment", alert.sentiment |> toSentimentString |> Sql.string
            "@date", alert.date |> Sql.date
            "@description", alert.description |> Sql.string
            "@strength", alert.strength |> Sql.decimal
            "@acknowledged", alert.acknowledged |> Sql.bool
            "@ticker", ticker
            "@corporateactiontype", corporateActionType
        ]

        cnnString
        |> Sql.connect
        |> Sql.executeTransaction [
            alertSql, [parameters]
            acknowledgementSql, [parameters]
        ]
        |> List.head

    let getAlerts() =
        let sql = @"SELECT a.alerttype, a.industry, a.screenerid, a.date, a.sentiment, a.description, a.strength,
                       COALESCE(ack.acknowledged, false) AS acknowledged, a.ticker, a.corporateactiontype
                FROM alerts a
                LEFT JOIN alert_acknowledgements ack ON a.identifier = ack.alert_identifier
                WHERE acknowledged = false"

        let alertMapper (reader:RowReader) =
            let alertType =
                match reader.stringOrNone "alerttype" with
                | Some (nameof(IndustryAlert)) -> IndustryAlert (reader.string "industry")
                | Some (nameof(ScreenerAlert)) -> ScreenerAlert (reader.int "screenerid")
                | Some (nameof(IndustryScreenerAlert)) -> IndustryScreenerAlert (reader.string "industry", reader.int "screenerid")
                | Some (nameof(CorporateActionAlert)) -> CorporateActionAlert (reader.string "ticker", reader.string "corporateactiontype")
                | _ -> failwith "Unknown alert type"
            {
                date = reader.dateTime "date"
                sentiment = reader.string "sentiment" |> toSentiment
                description = reader.string "description"
                strength = reader.decimal "strength"
                alertType = alertType
                acknowledged = reader.bool "acknowledged"
            }

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.execute alertMapper
        
    let deleteAlert (alert:Alert) =
        let sql = @"DELETE FROM alerts WHERE identifier = @identifier"
        
        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters ["@identifier", alert.identifier |> Sql.string]
        |> Sql.executeNonQuery
        
    let saveCorporateAction (action:CorporateAction) =
        let sql = @"INSERT INTO corporateactions (date, symbol, type, action)
                    VALUES (date(@date), @symbol, @type, @action)
                    ON CONFLICT (date, symbol, type) DO UPDATE
                    SET action = @action"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@date", action.Date |> Sql.string
            "@symbol", action.Symbol |> Sql.string
            "@type", action.Type |> Sql.string
            "@action", action.Action |> Sql.string
        ]
        |> Sql.executeNonQuery

    let saveCorporateActions (actions:CorporateAction seq) =
        actions |> Seq.map saveCorporateAction |> Seq.sum
    
    let private GetCorporateActionsByQuery sql parameters = task {
        let corporateActionMapper (reader:RowReader) = {
            Date = reader.dateTime "date" |> Utils.convertToDateString
            Symbol = reader.string "symbol"
            Type = reader.string "type"
            Action = reader.string "action"
        }
        
        return!
            cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters parameters
            |> Sql.executeAsync corporateActionMapper
    }
    
    let getCorporateActions() = 
        let sql = @"SELECT date, symbol, type, action FROM corporateactions ORDER BY date DESC"
        GetCorporateActionsByQuery sql []
    
    let getCorporateActionsForTicker (ticker:StockTicker.T) =
        let sql = @"SELECT date, symbol, type, action FROM corporateactions WHERE symbol = @symbol ORDER BY date DESC"
        GetCorporateActionsByQuery sql ["@symbol", ticker |> StockTicker.value |> Sql.string]