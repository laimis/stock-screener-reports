namespace FinvizScraper.Web.Handlers

module StockDashboard =
    open FinvizScraper.Core
    open FinvizScraper.Storage
    open FinvizScraper.Storage.Reports
    open FinvizScraper.Web.Shared
    open FinvizScraper.Web.Shared.Views
    open Giraffe.ViewEngine.Attributes
    open Giraffe.ViewEngine.HtmlElements
    open FinvizScraper.Web.Shared.Links


    let view (stock:Stock) (recentScreenerResults:list<ScreenerResultReportItem>) =

        let header = div [_class "content"] [
           h1 [] [
               stock.ticker |> StockTicker.value |> str
               " - " |> str
               stock.company |> str

               a [ 
                   stock.ticker |> StockTicker.value |> tradingViewLink |> _href
                   _class "is-pulled-right"
                   _target "_blank"
                ] [ str "Trading View" ]
           ]
           div [_class "is-size-5" ] [
               generateHref stock.sector (sectorLink stock.sector)
               str " / "
               generateHref stock.industry (industryLink stock.industry)
               str ", "
               str stock.country
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
                    td [] [ screenerResult.date.ToString("yyyy-MM-dd") |> str ]
                    td [] [ screenerResult.screenername |> str ]
                    td [] [ screenerResult.marketCap |> marketCapFormatted |> str ]
                    td [] [ screenerResult.price |> dollarFormatted |> str ]
                    td [] [ screenerResult.change |> percentFormatted |> str ]
                    td [] [ screenerResult.volume |> volumeFormatted |> str ]
                ]
            )

        [
            header
            tableHeader::results |> Views.fullWidthTable
        ]


    let handler ticker =
        let stockTicker = StockTicker.create ticker
        let stock = Storage.getStockByTicker stockTicker
        match stock with
        | Some stock ->
            let recentHits = getScreenerResultsForTicker stockTicker

            printf "Result count %i" (recentHits.Length)

            let nodes = view stock recentHits

            nodes |> Views.mainLayout "Stock Dashboard"
        | None -> 
            Views.notFound $"Stock with {ticker} symbol not found"
        