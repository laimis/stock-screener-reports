namespace FinvizScraper.Storage

module Storage =

    open Npgsql.FSharp
    open FinvizScraper.Core

    let private defaultLogger (message:string) = ()

    // TODO: add logger
    let mutable private cnnString = ""
    let configureConnectionString str =
        cnnString <- str

    let mutable private logger = defaultLogger
    let configureLogger log =
        logger <- log

    let private stockMapper (reader:RowReader) =
        {
            id = reader.int "id";
            ticker = reader.string "ticker" |> StockTicker.create;
            company = reader.string "name";
            sector = reader.string "sector";
            industry = reader.string "industry";
            country = reader.string "country";
        }

    let private industryUpdateMapper (reader:RowReader) : IndustryUpdate =
        {
            industry = reader.string "industry";
            date = reader.dateTime "date";
            days = reader.int "days";
            above = reader.int "above";
            below = reader.int "below";
        }

    let singleOrThrow message results =
        match results with
        | [] -> None
        | [x] -> Some x
        | _ -> raise (new System.Exception(message))

    let getStockByTicker (ticker:StockTicker.T) =
        let sql = "SELECT id,ticker,name,sector,industry,country FROM stocks WHERE ticker = @ticker"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters ["@ticker", ticker |> StockTicker.value |> Sql.string]
            |> Sql.execute stockMapper
            |> singleOrThrow "Expected single result for stock"

    let getStocksBySector (sector:string) =
        let sql = "SELECT id,ticker,name,sector,industry,country FROM stocks WHERE sector = @sector"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters ["@sector", sector |> Sql.string]
            |> Sql.execute stockMapper

    let getStocksByIndustry (industry:string) =
        let sql = "SELECT id,ticker,name,sector,industry,country FROM stocks WHERE industry = @industry"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters ["@industry", industry |> Sql.string]
            |> Sql.execute stockMapper
    
    // TODO: should we consider types for ticker, sectory, industry, country?
    let saveStock (ticker:StockTicker.T) name sector industry country =
        
        let sql = @"INSERT INTO stocks (ticker,name,sector,industry,country)
            VALUES (@ticker,@name,@sector,@industry,@country) RETURNING *"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                    "@ticker", ticker |> StockTicker.value |> Sql.string;
                    "@name", Sql.string name;
                    "@sector", Sql.string sector;
                    "@industry", Sql.string industry;
                    "@country", Sql.string country
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
        |> Sql.query "SELECT id,name,url FROM screeners"
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
        cnnString
        |> Sql.connect
        |> Sql.query "DELETE FROM screeners WHERE id = @id"
        |> Sql.parameters [ "@id", Sql.int screener.id ]
        |> Sql.executeNonQuery

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

    let saveScreenerResult screener date (stock:Stock) result =

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

    let getOrSaveStock (ticker:StockTicker.T) name sector industry country =
        let stockOrNone = getStockByTicker ticker
        match stockOrNone with
            | Some stock -> stock
            | None -> saveStock ticker name sector industry country

    let getOrSaveScreener (input:ScreenerInput) =
        let screenerOption = getScreenerByName input.name
        match screenerOption with
            | Some screener -> screener
            | None -> saveScreener input.name input.url

    let saveScreenerResults date (input:ScreenerInput,results:seq<ScreenerResult>) =
        
        let screener = getOrSaveScreener input
        
        deleteScreenerResults screener date |> ignore

        results
        |> Seq.map (fun result ->
            let stock = getOrSaveStock result.ticker result.company result.sector result.industry result.country
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

    let saveIndustryUpdates date  (industry,days,above:int,below:int) =
        let sql = @"
            DELETE FROM industryupdates WHERE industry = @industry AND date = date(@date) AND days = @days;

            INSERT INTO industryupdates
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

    let getIndustryTrends days industry =
        let sql = @"
            SELECT industry,date,days,above,below
            FROM industryupdates
            WHERE industry = @industry
            AND days = @days
            ORDER BY date"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@industry", Sql.string industry;
            "@days", Sql.int days;
        ]
        |> Sql.execute industryUpdateMapper

    let getIndustryUpdates days date =
        let sql = @"
            SELECT industry,date,days,above,below FROM industryupdates
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
        |> Sql.execute industryUpdateMapper

    let getMostRecentIndustryTrends days industry =
        let sql = @"
            SELECT industry,date,days,above,below FROM industryupdates
            WHERE industry = @industry
            AND days = @days
            AND date = (SELECT MAX(date) FROM industryupdates WHERE industry = @industry AND days = @days)"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@industry", Sql.string industry;
            "@days", Sql.int days;
        ]
        |> Sql.execute industryUpdateMapper
        |> singleOrThrow "More than one industry trend for the same industry and days"

    let getIndustryUpdatesLatestDate() =
        cnnString
        |> Sql.connect
        |> Sql.query "SELECT MAX(date) as date FROM industryupdates"
        |> Sql.executeRow (fun reader -> reader.dateTime "date")

    let saveJobStatus (jobName:JobName) (timestamp : System.DateTime) (status:JobStatus) message =
        let toJobNameString jobName =
            match jobName with
                | ScreenerJob _ -> "screenerjob"
                | IndustryTrendsJob _ -> "industrytrendsjob"

        let toJobStatusString status =
            match status with
                | Success _ -> "success"
                | Failure _ -> "failure"

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
            "@timestamp", Sql.timestamptz timestamp;
            "@status", status |> toJobStatusString |> Sql.string;
            "@message", Sql.string message;
        ]
        |> Sql.executeNonQuery