namespace FinvizScraper.Web.Handlers

module StockDashboard =
    open FinvizScraper.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open Giraffe.ViewEngine.Attributes
    open FinvizScraper.Storage
    open FinvizScraper.Core
    open FinvizScraper.Storage.Reports

    let view (stock:Stock) (recentScreenerResults:list<ScreenerResultReportItem>) =

        let header = div [_class "content"] [
           h1 [] [str stock.ticker]
           div [_class "columns"] [
               div [_class "column"] [
                   str stock.company
               ]
               div [_class "column"] [
                   str stock.sector
               ]
               div [_class "column"] [
                   str stock.industry
               ]
               div [_class "column"] [
                   str stock.country
               ]
           ]
        ]

        let tableHeader =
            tr [] [
                th [] [str "date"]
                th [] [str "screener"]
                th [] [str "market cap"]
                th [] [str "price"]
                th [] [str "change"]
                th [] [str "volume"]
            ]
        
        let results = 
            recentScreenerResults |> List.map (fun screenerResult ->
                tr [] [
                    td [] [str (screenerResult.date.ToString("yyyy-MM-dd"))]
                    td [] [str (screenerResult.screenername)]
                    td [] [str (screenerResult.marketCap.ToString())]
                    td [] [str (screenerResult.price.ToString())]
                    td [] [str (screenerResult.change.ToString())]
                    td [] [str (screenerResult.volume.ToString())]
                ]
            )

        [
            header
            tableHeader::results |> Views.fullWidthTable
        ]


    let handler ticker =
        let stock = Storage.getStockByTicker ticker
        match stock with
        | Some stock ->
            let recentHits = getScreenerResultsForTicker stock.ticker

            printf "Result count %i" (recentHits.Length)

            let nodes = view stock recentHits

            nodes |> Views.mainLayout "Stock Dashboard"
        | None -> 
            Views.notFound $"Stock with {ticker} symbol not found"
        