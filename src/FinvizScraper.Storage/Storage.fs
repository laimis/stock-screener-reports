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
            ticker = reader.string "ticker";
            company = reader.string "name";
            sector = reader.string "sector";
            industry = reader.string "industry";
            country = reader.string "country";
        }

    let getStockByTicker ticker =
        let sql = "SELECT id,ticker,name,sector,industry,country FROM stocks WHERE ticker = @ticker"

        let results =
            cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters ["@ticker", Sql.string ticker]
            |> Sql.execute stockMapper

        match results with
            | [] -> None
            | [stock] -> Some stock
            | _ -> raise (new System.Exception "Expected single result for stock")
    
    // TODO: should we consider types for ticker, sectory, industry, country?
    let saveStock (ticker:string) name sector industry country =
        
        let sql = @"INSERT INTO stocks (ticker,name,sector,industry,country)
            VALUES (@ticker,@name,@sector,@industry,@country) RETURNING *"

        cnnString
            |> Sql.connect
            |> Sql.query sql
            |> Sql.parameters [
                    "@ticker", Sql.string ticker;
                    "@name", Sql.string name;
                    "@sector", Sql.string sector;
                    "@industry", Sql.string industry;
                    "@country", Sql.string country
                ]
            |> Sql.executeRow stockMapper

    let deleteStock (stock:Stock) =
        let sql = "DELETE FROM stocks WHERE id = @stockId"

        cnnString
        |> Sql.connect
        |> Sql.query sql
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

        let results =
            cnnString 
            |> Sql.connect
            |> Sql.query "SELECT id,name,url FROM screeners WHERE name = @name"
            |> Sql.parameters [ "@name", Sql.string name ]
            |> Sql.execute screenerMapper
        
        match results with
        | [] -> None
        | [a] -> Some a
        | _ -> raise (new System.Exception("More than one screener with the same name"))
        
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
            (screenerid,date,stockId,price,change,volume)
            VALUES
            (@screenerId,date(@date),@stockId,@price,@change,@volume)"

        cnnString
        |> Sql.connect
        |> Sql.query sql
        |> Sql.parameters [
            "@screenerId", Sql.int screener.id;
            "@date", Sql.string date;
            "@stockId", Sql.int stock.id;
            "@price", Sql.decimal result.price;
            "@change", Sql.decimal result.change;
            "@volume", Sql.int result.volume
        ]
        |> Sql.executeNonQuery

    let getOrSaveStock ticker name sector industry country =
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