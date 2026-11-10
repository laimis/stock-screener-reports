namespace StockScreenerReports.FinvizClient

module FinvizClient =
    open StockScreenerReports.Core

    let mutable outputFunc = (fun _ -> ())

    let private SLEEP_BETWEEN_REQUESTS_MS = 250

    let setOutputFunc (f:string -> unit) =
        outputFunc <- f
        FinvizParsing.setOutputFunc f

    let private fetchUrl (url:string) =
        // make ure that we sleep a bit before each request
        System.Threading.Thread.Sleep(SLEEP_BETWEEN_REQUESTS_MS)
        url |> outputFunc 
        let web = HtmlAgilityPack.HtmlWeb()
        web.Load(url)

    let getResults url =
        let rec fetchPage offset (results:list<ScreenerResult>) =
            let urlToFetch = url + "&r=" + offset.ToString()
            let htmlDoc = fetchUrl urlToFetch

            outputFunc urlToFetch
            
            let page = 
                htmlDoc
                |> FinvizParsing.parseScreenerHtml
                |> Seq.toList

            if page.IsEmpty && offset = 1 then
                let title =
                    match htmlDoc.DocumentNode.SelectSingleNode("//title") with
                    | null -> "<no title>"
                    | t -> t.InnerText.Trim()
                outputFunc $"No rows parsed from first page of {urlToFetch} (page title: '{title}', length: {htmlDoc.DocumentNode.OuterLength})"

            // finviz returns the last page again whenever a page past the end is requested
            // (e.g. total of 27, r=41 returns rows 21-27 again; with a total of 40 it returns a full page of 20).
            // Also, when sorting by something live like change, a ticker can move between pages while we are
            // fetching and show up twice. So only keep tickers we have not seen, and stop when a page adds nothing new.
            let seen = results |> List.map (fun r -> r.ticker) |> Set.ofList

            let newRows =
                page
                |> List.filter (fun r -> seen.Contains r.ticker |> not)
                |> List.distinctBy (fun r -> r.ticker)

            let combined = List.append results newRows

            if newRows.IsEmpty || page.Length < 20 then
                combined
            else
                fetchPage (offset + 20) combined

        fetchPage 1 []

    let getResultCount (url:string) =
        try
            url |> fetchUrl |> FinvizParsing.parseResultCount
        with ex ->
            raise (System.Exception($"Failed to get result count for url {url}: {ex.Message}", ex))

    let getResultCountForIndustryAboveAndBelowSMA (sma:SMA) industry =
        let cleaned = industry |> Utils.cleanIndustry

        let days = sma.Interval
        
        let fetchCountWithTA ta =
            let url = $"https://finviz.com/screener.ashx?v=111&f=ind_{cleaned},{ta}"
            try
                url |> getResultCount
            with ex ->
                raise (System.Exception($"Industry '{industry}' (cleaned: '{cleaned}'): {ex.Message}", ex))
        
        let above = $"ta_sma%i{days}_pa" |> fetchCountWithTA
        let below = $"ta_sma%i{days}_pb" |> fetchCountWithTA

        (above,below)

    let getEarnings() =
        
        let before = getResults "https://finviz.com/screener.ashx?v=111&s=n_earningsbefore" |> List.map (fun r -> r.ticker, BeforeMarket)
        let after = getResults "https://finviz.com/screener.ashx?v=111&s=n_earningsafter" |> List.map (fun r -> r.ticker, AfterMarket)

        List.concat [before; after]
        
    let getResultCountForCountryAboveAndBelowSMA (sma:SMA) country =
        let cleaned = country |> Utils.cleanCountry
        
        let fetchCountWithTA ta =
            let url = $"https://finviz.com/screener.ashx?v=111&f=geo_{cleaned},{ta}"
            try
                url |> getResultCount
            with ex ->
                raise (System.Exception($"Country '{country}' (cleaned: '{cleaned}'): {ex.Message}", ex))
            
        let interval = sma.Interval
        
        let above = $"ta_sma%i{interval}_pa" |> fetchCountWithTA
        let below = $"ta_sma%i{interval}_pb" |> fetchCountWithTA
        
        (above,below)