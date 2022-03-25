namespace FinvizScraper

module Storage =

    open Npgsql
    open Npgsql.FSharp

    // TODO: see how F# does db code with Npgsql, and perhaps dapper
    // TODO: move to config
    let cnnString = "Server=localhost;Port=5432;Database=finviz;User Id=finviz;Password=finviz;Include Error Detail=true"

    let getStockByTicker ticker =
        let sql = "SELECT id,ticker,name,sector,industry,country FROM stocks WHERE ticker = :ticker"

        // Connect to the SQL database
        use conn = new NpgsqlConnection(cnnString)
        conn.Open()

        // Execute the query
        let cmd = new NpgsqlCommand(sql, conn)
        let param = new NpgsqlParameter(parameterName="ticker", value=ticker)
        cmd.Parameters.Add(param) |> ignore
        let reader = cmd.ExecuteReader()

        match reader.Read() with
            | true ->
                let stock = {
                    id = reader.GetInt32(0);
                    ticker = reader.GetString(1);
                    company = reader.GetString(2);
                    sector = reader.GetString(3);
                    industry = reader.GetString(4);
                    country = reader.GetString(5);
                }
                Some (stock)
            | false ->
                None
    
    // TODO: should we consider types for ticker, sectory, industry, country?
    let saveStock (ticker:string) name sector industry country =
        use conn = new NpgsqlConnection(cnnString)
        conn.Open()

        let sql = "INSERT INTO stocks (ticker,name,sector,industry,country) VALUES (:ticker,:name,:sector,:industry,:country) RETURNING id"

        let cmd = new NpgsqlCommand(sql, conn)
        let tickerParam = new NpgsqlParameter(parameterName="ticker", value=ticker.ToUpper())
        let nameParam = new NpgsqlParameter(parameterName="name", value=name)
        let sectorParam = new NpgsqlParameter(parameterName="sector", value=sector)
        let industryParam = new NpgsqlParameter(parameterName="industry", value=industry)
        let countryParam = new NpgsqlParameter(parameterName="country", value=country)
        cmd.Parameters.Add(tickerParam)     |> ignore
        cmd.Parameters.Add(nameParam)       |> ignore
        cmd.Parameters.Add(sectorParam)     |> ignore
        cmd.Parameters.Add(industryParam)   |> ignore
        cmd.Parameters.Add(countryParam)    |> ignore
        let id = cmd.ExecuteScalar() :?> int

        {
            id = id;
            ticker = ticker;
            company = name;
            sector = sector;
            industry = industry;
            country = country;
        }

    // TODO: should we consider type for stockid?
    let deleteStock stockId =
        use conn = new NpgsqlConnection(cnnString)
        conn.Open()

        let sql = "DELETE FROM stocks WHERE id = :stockId"

        let cmd = new NpgsqlCommand(sql, conn)
        let param = new NpgsqlParameter(parameterName="stockId", value=stockId)
        cmd.Parameters.Add(param) |> ignore
        cmd.ExecuteNonQuery()

    let saveScreener name url =
        let id =
            cnnString
            |> Sql.connect
            |> Sql.query "INSERT INTO screeners (name,url) VALUES (@name,@url) RETURNING id"
            |> Sql.parameters [
                "@name", Sql.string name;
                "@url", Sql.string url
            ]
            |> Sql.executeRow (fun reader -> reader.int("id"))

        {
            id = id;
            name = name;
            url = url;
        }

    let getScreenerByName name = 

        let results =
            cnnString 
            |> Sql.connect
            |> Sql.query "SELECT id,name,url FROM screeners WHERE name = @name"
            |> Sql.parameters [ "@name", Sql.string name ]
            |> Sql.execute (fun reader ->
                {
                    id = reader.int "id";
                    name = reader.string "name";
                    url = reader.string "url";
                }
            )
        
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

    let deleteScreenerResults screenerId (date:string) =
        use conn = new NpgsqlConnection(cnnString)
        conn.Open()

        let sql = "DELETE FROM screenerresults WHERE screenerid = :screenerId AND date = date(:date)"

        let cmd = new NpgsqlCommand(sql, conn)
        let screenerIdParam = new NpgsqlParameter(parameterName="screenerId", value=screenerId)
        let dateParam = new NpgsqlParameter(parameterName="date", value=date)
        cmd.Parameters.Add(screenerIdParam) |> ignore
        cmd.Parameters.Add(dateParam)    |> ignore
        cmd.ExecuteNonQuery()

    let saveScreenerResult screenerId date stock result =

        System.Console.WriteLine($"db saveScreenerResult: {screenerId} {date} {stock.ticker} {result.price}")
        use conn = new NpgsqlConnection(cnnString)
        conn.Open()

        let sql = @"INSERT INTO screenerresults
            (screenerid,date,stockId,price)
            VALUES
            (:screenerId,date(:date),:stockId,:price)"

        let cmd = new NpgsqlCommand(sql, conn)
        let screenerIdParam = new NpgsqlParameter(parameterName="screenerId", value=screenerId)
        let stockIdParam = new NpgsqlParameter(parameterName="stockId", value=stock.id)
        let dateParam = new NpgsqlParameter(parameterName="date", value=date)
        let resultsParam = new NpgsqlParameter(parameterName="price", value=result.price)
        cmd.Parameters.Add(screenerIdParam) |> ignore
        cmd.Parameters.Add(stockIdParam)    |> ignore
        cmd.Parameters.Add(dateParam)    |> ignore
        cmd.Parameters.Add(resultsParam)    |> ignore
        cmd.ExecuteNonQuery()

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
        
        let deleted = deleteScreenerResults screener.id date

        System.Console.WriteLine($"deleted {deleted} for {screener.id} {date}")
        System.Console.WriteLine($"saving {results |> Seq.length} results for {screener.id} {date}")

        results
        |> Seq.map (fun result ->
            let stock = getOrSaveStock result.ticker result.company result.sector result.industry result.country
            (screener.id,stock,result)
        )
        |> Seq.iter (fun (screenerId,stock,result) ->
            saveScreenerResult screenerId date stock result |> ignore
        )