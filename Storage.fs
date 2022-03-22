namespace FinvizScraper

module Storage =

    open Npgsql

    let cnnString = "Server=localhost;Port=5432;Database=finviz;User Id=finviz;Password=finviz"

    let getScreenerByName name = 
        let sql = "SELECT id,name,url FROM screeners WHERE name = :name"
        use conn = new NpgsqlConnection(cnnString)
        conn.Open()

        // Execute the query
        let cmd = new NpgsqlCommand(sql, conn)
        let param = new NpgsqlParameter(parameterName="name", value=name)
        cmd.Parameters.Add(param) |> ignore
        let reader = cmd.ExecuteReader()

        match reader.Read() with
            | true ->
                let id = reader.GetInt32(0)
                let ticker = reader.GetString(1)
                Some (id,ticker)
            | false ->
                None
        
    let getStockByTicker ticker =
        let sql = "SELECT id,ticker FROM stocks WHERE ticker = :ticker"

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
                let id = reader.GetInt32(0)
                let ticker = reader.GetString(1)
                Some (id,ticker)
            | false ->
                None
    
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

        (id,ticker)

    let deleteStock stockId =
        use conn = new NpgsqlConnection(cnnString)
        conn.Open()

        let sql = "DELETE FROM stocks WHERE id = :stockId"

        let cmd = new NpgsqlCommand(sql, conn)
        let param = new NpgsqlParameter(parameterName="stockId", value=stockId)
        cmd.Parameters.Add(param) |> ignore
        cmd.ExecuteNonQuery()

    let saveScreener name url =
        use conn = new NpgsqlConnection(cnnString)
        conn.Open()

        let sql = "INSERT INTO screeners (name,url) VALUES (:name,:url) RETURNING id"

        let cmd = new NpgsqlCommand(sql, conn)
        let nameParam = new NpgsqlParameter(parameterName="name", value=name)
        let urlParam = new NpgsqlParameter(parameterName="url", value=url)
        cmd.Parameters.Add(nameParam)     |> ignore
        cmd.Parameters.Add(urlParam)      |> ignore
        let id = cmd.ExecuteScalar() :?> int

        (id,name)

    let deleteScreener id =
        use conn = new NpgsqlConnection(cnnString)
        conn.Open()

        let sql = "DELETE FROM screeners WHERE id = :id"

        let cmd = new NpgsqlCommand(sql, conn)
        let param = new NpgsqlParameter(parameterName="id", value=id)
        cmd.Parameters.Add(param) |> ignore
        cmd.ExecuteNonQuery()

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

    let saveScreenerResult screenerId date stockId results =
        use conn = new NpgsqlConnection(cnnString)
        conn.Open()

        let sql = @"INSERT INTO screenerresults
            (screenerid,date,stockId,price)
            VALUES
            (:screenerId,date(:date),:stockId,:price)"

        let cmd = new NpgsqlCommand(sql, conn)
        let screenerIdParam = new NpgsqlParameter(parameterName="screenerId", value=screenerId)
        let stockIdParam = new NpgsqlParameter(parameterName="stockId", value=stockId)
        let dateParam = new NpgsqlParameter(parameterName="date", value=date)
        let resultsParam = new NpgsqlParameter(parameterName="price", value=results.price)
        cmd.Parameters.Add(screenerIdParam) |> ignore
        cmd.Parameters.Add(stockIdParam)    |> ignore
        cmd.Parameters.Add(dateParam)    |> ignore
        cmd.Parameters.Add(resultsParam)    |> ignore
        cmd.ExecuteNonQuery()

    let getOrSaveStock ticker name sector industry country =
        let stockOption = getStockByTicker ticker
        match stockOption with
            | Some (id,ticker) ->
                (id,ticker)
            | None ->
                let (id,ticker) = saveStock ticker name sector industry country
                (id,ticker)

    let getOrSaveScreener (input:ScreenerInput) =
        let screenerOption = getScreenerByName input.name
        match screenerOption with
            | Some screener ->
                screener
            | None ->
                let (id, name) = saveScreener input.name input.url
                (id,name)

    let saveScreenerResults date (input:ScreenerInput,results:seq<ScreenerResult>) =
        
        System.Console.WriteLine("saving to db... " + input.name)
        System.Console.WriteLine("results " + (results |> Seq.length).ToString())
        // for each result, we want to make sure we save and load stock id

        let (screenerId,_) = getOrSaveScreener input
        
        deleteScreenerResults screenerId date |> ignore

        results
        |> Seq.map (fun result ->
            let (stockId,_) = getOrSaveStock result.ticker result.company result.sector result.industry result.country
            (screenerId,stockId,result)
        )
        |> Seq.iter (fun (screenerId,stockId,result) ->
            saveScreenerResult screenerId date stockId result |> ignore
        )