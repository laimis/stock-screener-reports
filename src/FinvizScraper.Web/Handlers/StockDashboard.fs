namespace FinvizScraper.Web.Handlers

module StockDashboard =
    open FinvizScraper.Core
    open FinvizScraper.Storage
    open FinvizScraper.Storage.Reports
    open FinvizScraper.Web.Shared.Views
    open Giraffe.ViewEngine.Attributes
    open Giraffe.ViewEngine.HtmlElements
    open FinvizScraper.Web.Shared.Links


    let private renderStockInternal (stock:Stock) =

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

        let screenerResultToRow screenerResult =
            tr [] [
                td [] [ screenerResult.date.ToString("yyyy-MM-dd") |> str ]
                td [] [ 
                    (screenerResult.screenerid,screenerResult.screenername) |> generateScreenerTags
                ]
                td [] [ screenerResult.marketCap |> marketCapFormatted |> str ]
                td [] [ screenerResult.price |> dollarFormatted |> str ]
                td [] [ screenerResult.change |> percentFormatted |> str ]
                td [] [ screenerResult.volume |> volumeFormatted |> str ]
            ]

        let days = 30

        let recentScreenerResults = getScreenerResultsForTickerDayRange stock.ticker days

        // group recent screenerresults by date
        let recentScreenerResultsByDate = 
            recentScreenerResults
            |> List.groupBy (fun screenerResult -> screenerResult.date)
            |> Map.ofList

        let rows = 
            FinvizScraper.Web.Shared.Logic.businessDatesWithZeroPairs days
            |> List.rev
            |> List.collect (fun (date,_) ->
                let screenerResults = Map.tryFind date recentScreenerResultsByDate
                match screenerResults with
                    | Some l -> l |> List.map screenerResultToRow
                    | None -> [tr [] [td [_colspan "6"] [date.ToString("yyyy-MM-dd") |> str]]]
            )

        // another table that's just all the screener hits without day limit in case we need to see older ones
        let allScreenerResults = getScreenerResultsForTicker stock.ticker 100
        let allScreenerResultsRows = allScreenerResults |> List.map screenerResultToRow

        [
            header
            tableHeader::rows |> fullWidthTable
            div [] [h2 [] [str "All Screener Results"]]
            tableHeader::allScreenerResultsRows |> fullWidthTable
        ]

    let renderTicker (stock:Stock) =
        
        let view = renderStockInternal stock

        let pageTitle = (stock.ticker |> StockTicker.value) + " - " + stock.company

        view |> mainLayout pageTitle

    let handler ticker =
        let stockTicker = StockTicker.create ticker
        let stock = Storage.getStockByTicker stockTicker
        match stock with
        | Some stock ->
            renderTicker stock
        | None -> 
            notFound $"Stock with {ticker} symbol not found"