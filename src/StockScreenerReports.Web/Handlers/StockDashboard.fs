namespace FinvizScraper.Web.Handlers

module StockDashboard =
    open FinvizScraper.Core
    open FinvizScraper.Storage
    open FinvizScraper.Storage.Reports
    open FinvizScraper.Web.Shared.Views
    open Giraffe.ViewEngine.Attributes
    open Giraffe.ViewEngine.HtmlElements
    open FinvizScraper.Web.Shared.Links
    open FinvizScraper.Web.Shared


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
                "date" |> toSortableHeaderCell
                "screener" |> toSortableHeaderCell
                "market cap" |> toSortableHeaderCell
                "price" |> toSortableHeaderCell
                "change" |> toSortableHeaderCell
                "volume" |> toSortableHeaderCell
            ]

        let screenerResultToRow screenerResult =
            tr [] [
                td [] [ screenerResult.date |> Utils.convertToDateString |> str ]
                td [] [ 
                    (screenerResult.screenerid,screenerResult.screenername) |> generateScreenerTags
                ]
                td [] [ screenerResult.marketCap |> marketCapFormatted |> str ]
                td [] [ screenerResult.price |> dollarFormatted |> str ]
                td [] [ screenerResult.change |> percentFormatted |> str ]
                td [] [ screenerResult.volume |> volumeFormatted |> str ]
            ]

        let days = FinvizConfig.dayRange

        let businessDays = Logic.businessDatesWithZeroPairs days

        // group recent screenerresults by date
        let recentScreenerResultsByScreener = 
            days
            |> getScreenerResultsForTickerDayRange stock.ticker
            |> List.groupBy (fun screenerResult -> (screenerResult.screenerid, screenerResult.screenername))
            |> Map.ofList
            
        let labels = businessDays |> List.map (fun (date, _) -> date |> Utils.convertToDateString)

        let datasets:list<Charts.DataSet<int>> = 
            recentScreenerResultsByScreener
            |> Map.toList
            |> List.map( fun ((screenerid,screenername),results) ->
                let data =
                    businessDays
                    |> List.map (fun (date, _) ->
                        let resultsByDate =
                            results
                            |> List.groupBy (fun screenerResult -> screenerResult.date)
                            |> Map.ofList

                        let m = resultsByDate |> Map.tryFind date

                        match m with
                        | Some list -> list.Length
                        | None -> 0
                    )
                
                {
                    data = data
                    title = screenername
                    color = screenerid |> FinvizConfig.getBackgroundColorForScreenerId
                }
            )

        let chart = Charts.generateChartElements "Screener hits" Charts.Bar (Some 1) Charts.smallChart labels datasets

        // another table that's just all the screener hits without day limit in case we need to see older ones
        let allScreenerResults = getScreenerResultsForTicker stock.ticker 100
        let allScreenerResultsRows = allScreenerResults |> List.map screenerResultToRow

        header::chart @ [
            div [] [h2 [] [str "All Screener Results"]]
            tableHeader::allScreenerResultsRows |> fullWidthTable
        ]

    let handler ticker =
        let stockTicker = StockTicker.create ticker
        let stock = Storage.getStockByTicker stockTicker
        match stock with
        | Some stock ->
            let view = renderStockInternal stock
            let pageTitle = (stock.ticker |> StockTicker.value) + " - " + stock.company
            view |> mainLayout pageTitle
        | None -> 
            notFound $"Stock with {ticker} symbol not found"