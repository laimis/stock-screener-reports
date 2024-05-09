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

            // check if the page has only one element and offset is greater than one
            // this potentially could be repeated result, finviz just returns the last result
            // whenever a new page is requested. When total results is something like 20 or 40
            // we don't know that this is the end and just keep fetching
            let isLastPage = page.Length = 1 && offset > 1
            let index = results |> List.tryFindIndex (fun r -> r.ticker.Equals(page.Item(0).ticker))

            if index.IsSome && isLastPage then
                results
            else
                match page with 
                    | c when c.Length = 20 -> fetchPage (offset + 20) (List.append results page)
                    | _ -> (List.append results page)

        fetchPage 1 []

    let getResultCount url =
        url |> fetchUrl |> FinvizParsing.parseResultCount

    let getResultCountForIndustryAboveAndBelowSMA (sma:SMA) industry =
        let cleaned = industry |> Utils.cleanIndustry

        let days = sma.Interval
        
        let fetchCountWithTA ta =
            let url = $"https://finviz.com/screener.ashx?v=111&f=ind_{cleaned},{ta}"
            url |> getResultCount
        
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
            url |> getResultCount
            
        let interval = sma.Interval
        
        let above = $"ta_sma%i{interval}_pa" |> fetchCountWithTA
        let below = $"ta_sma%i{interval}_pb" |> fetchCountWithTA
        
        (above,below)
        
    let getCorporateActions() =
        
        let doc = fetchUrl "https://stockanalysis.com/actions/"
        
        let tableRows =
            doc.DocumentNode.Descendants("table")
            |> Seq.find (fun t -> t.HasClass("svelte-1yyv6eq"))
            |> _.Descendants("tbody")
            |> Seq.head
            |> _.Descendants("tr")

        tableRows
        |> Seq.map (fun row ->
            let cells = row.Descendants("td") |> Seq.toList
            {
                Date = cells.[0].InnerText.Trim()
                Symbol = cells.[1].InnerText.Trim()
                Type = cells.[2].InnerText.Trim()
                Action = cells.[3].InnerText.Trim()
            })
        |> Seq.toList