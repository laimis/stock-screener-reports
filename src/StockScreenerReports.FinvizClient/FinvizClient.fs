namespace StockScreenerReports.FinvizClient

module FinvizClient =
    open StockScreenerReports.Core

    let mutable outputFunc = (fun str -> ())

    let private SLEEP_BETWEEN_REQUESTS_MS = 250

    let setOutputFunc (f:string -> unit) =
        outputFunc <- f
        FinvizParsing.setOutputFunc f

    let private fetchScreenerHtml (url:string) =
        // make ure that we sleep a bit before each request
        System.Threading.Thread.Sleep(SLEEP_BETWEEN_REQUESTS_MS)
        url |> outputFunc 
        let web = HtmlAgilityPack.HtmlWeb()
        web.Load(url)

    let getResults url =
        let rec fetchPage offset (results:list<ScreenerResult>) =
            let urlToFetch = url + "&r=" + offset.ToString()
            let htmlDoc = fetchScreenerHtml urlToFetch

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
        url |> fetchScreenerHtml |> FinvizParsing.parseResultCount

    let getResultCountForIndustryAboveAndBelowSMA days industry =
        let cleaned = industry |> Utils.cleanIndustry

        let fetchCountWithTA ta =
            let url = $"https://finviz.com/screener.ashx?v=111&f=ind_{cleaned},{ta}"
            url |> getResultCount
        
        let above = $"ta_sma{days}_pa" |> fetchCountWithTA
        let below = $"ta_sma{days}_pb" |> fetchCountWithTA

        (above,below)

    let getEarnings() =
        
        let before = getResults "https://finviz.com/screener.ashx?v=111&s=n_earningsbefore" |> List.map (fun r -> r.ticker, BeforeMarket)
        let after = getResults "https://finviz.com/screener.ashx?v=111&s=n_earningsafter" |> List.map (fun r -> r.ticker, AfterMarket)

        List.concat [before; after]