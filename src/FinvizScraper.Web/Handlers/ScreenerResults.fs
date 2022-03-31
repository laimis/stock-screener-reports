namespace FinvizScraper.Web.Handlers

module ScreenerResults =

    open Giraffe.ViewEngine
    open FinvizScraper.Storage.Reports

    let screenerResultToTr (result:ScreenerResultReportItem) =
        
        let toTd input =
            td [] [
                str input
            ]
        
        let toTdWithHref href =
            td [] [
                a [
                    _href href
                    _target "_blank"
                ] [
                    str "link"
                ]
            ]

        let rowAttributes = [_height "50px"]

        tr rowAttributes [
            toTd result.ticker
            toTd result.name
            toTd result.sector
            toTd result.industry
            toTd result.country
            // toTd result.marketCap TODO: add this back once we start storing market cap with each screener result
            toTd (string result.price)
            toTd (string result.change)
            toTd (string (result.volume.ToString("N0")))
            toTdWithHref $"https://tradingview.com/chart/kQn4rgoA/?symbol={result.ticker}"
        ]

    let headerRow =
        let headers = [ 
            "Ticker"
            "Company"
            "Sector"
            "Industry"
            "Country" 
            // "Market Cap" TODO: add this back once we start storing market cap with each screener result
            "Price"
            "Change"
            "Volume"
            "Link"
        ]

        let toHeader title =
            th [] [str title]

        let headerCells = headers |> List.map toHeader

        tr [] headerCells

    let private view (screener:FinvizScraper.Core.Screener) (results:list<ScreenerResultReportItem>) =
        let tickerRows = results |> List.map screenerResultToTr

        let resultTable = table [Shared.fullWidthTableAttributes] (headerRow::tickerRows)

        let content = [
            div [_class "content"] [
                h1 [] [
                    str ("Screener: " + screener.name)
                ]
                div [_class "block"] [
                    str ("Total Results: " + (string results.Length))
                ]
                div [_class "block"] [
                    a [ 
                        _href screener.url
                        _target "_blank"
                    ] [
                        str "View on Finviz"
                    ]
                ]
            ]
            // div [_class "columns"] breakdownDivs
            div [_class "block"] [resultTable]
        ]
        
        content |> Shared.mainLayout $"Screener: {screener.name}"

    let handler ((id:int),(date:string))  = 
        
        // get screeners, render them in HTML
        let byIdOption = FinvizScraper.Storage.Storage.getScreenerById id
        match byIdOption with
        | Some screener -> 
            let screenerResults = getScreenerResults screener.id date
            let view      = view screener screenerResults
            Giraffe.Core.htmlView view
        | None ->
            let view = 
                div [_class "content"] [
                    h1 [] [
                        str "Screener not found"
                    ]
                ]
            Giraffe.Core.htmlView view