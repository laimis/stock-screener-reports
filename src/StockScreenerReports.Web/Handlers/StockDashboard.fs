namespace StockScreenerReports.Web.Handlers

module StockDashboard =
    open StockScreenerReports.Core
    open StockScreenerReports.Storage
    open StockScreenerReports.Storage.Reports
    open StockScreenerReports.Web.Shared.Views
    open Giraffe.ViewEngine.Attributes
    open Giraffe.ViewEngine.HtmlElements
    open StockScreenerReports.Web.Shared.Links
    open StockScreenerReports.Web.Shared

    let createHeader (stock:Stock) =

        let lastUpdate = 
            match stock.lastUpdate with
            | Some date -> date |> Utils.convertToDateString
            | None -> "N/A"

        div [_class "content"] [
           h1 [] [
               $"{(stock.ticker |> StockTicker.value)} - {stock.company}" |> str
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
               div [_class "is-pulled-right"] [
                   str $"Last updated: {lastUpdate}"
               ]
            ]
        ]

    let createScreenerResultsSection results =
        let screenerResultToRow screenerResult =
            let dateStr = screenerResult.date |> Utils.convertToDateString
            let screenerLink = dateStr |> screenerResultsLink screenerResult.screenerid

            [
                LinkColumn(dateStr, screenerLink)
                NodeColumn((screenerResult.screenerid,screenerResult.screenername) |> generateScreenerTags)
                StringColumn(screenerResult.marketCap |> marketCapFormatted)
                StringColumn(screenerResult.price |> dollarFormatted )
                StringColumn(screenerResult.change |> percentFormatted )
                StringColumn(screenerResult.volume |> volumeFormatted )
            ] |> toTr

        let allScreenerResultsRows = results |> List.map screenerResultToRow

        let tableHeader = [
            "Date"
            "Screener"
            "Market Cap"
            "Price"
            "Change"
            "Volume"
        ]

        [
            section [_class "content"] [h2 [] [str "All Screener Results"]]
            allScreenerResultsRows |> fullWidthTableWithSortableHeaderCells tableHeader
        ]

    let createScreenerHitChart tickerScreenerResults =
        let dateRange = ReportsConfig.dateRange()

        let businessDays = ReportsConfig.listOfBusinessDates dateRange

        // group recent screenerresults by date
        let recentScreenerResultsByScreener = 
            tickerScreenerResults
            |> List.groupBy (fun screenerResult -> (screenerResult.screenerid, screenerResult.screenername))
            |> Map.ofList
            
        let labels = businessDays |> Seq.map (fun (date) -> date |> Utils.convertToDateString)

        let datasets:list<Charts.DataSet<int>> = 
            recentScreenerResultsByScreener
            |> Map.toList
            |> List.map( fun ((screenerid,screenername),results) ->
                let data =
                    businessDays
                    |> Seq.map (fun date ->
                        let resultsByDate =
                            results
                            |> List.groupBy (fun screenerResult -> screenerResult.date |> Utils.convertToDateString )
                            |> Map.ofList

                        let m = resultsByDate |> Map.tryFind (date |> Utils.convertToDateString)

                        match m with
                        | Some list -> list.Length
                        | None -> 0
                    )
                    |> Seq.toList
                
                {
                    data = data
                    title = screenername
                    color = screenerid |> ReportsConfig.getBackgroundColorForScreenerId
                }
            )

        [
            section [_class "content"] [
                h2 [] [str "Screener Hits"]
                div [] (Charts.generateChartElements "Screener hits" Charts.Bar (Some 1) Charts.smallChart labels datasets)
            ]
        ]
        


    let private renderStockInternal (stock:Stock) dateRangeScreenerHits lastNScreenerHits =

        let header = stock |> createHeader

        let screenerHitChart = dateRangeScreenerHits |> createScreenerHitChart 

        let screenerResultsSection = lastNScreenerHits |> createScreenerResultsSection

        header::screenerHitChart @ screenerResultsSection

    let handler ticker =
        let stock =
            ticker
            |> StockTicker.create
            |> Storage.getStockByTicker

        match stock with
        | Some stock ->
            
            let dateRangeScreenerHits = (ReportsConfig.dateRangeAsStrings()) |> getScreenerResultsForTickerDayRange stock.ticker
            let lastNScreenerHits = stock.ticker |> getScreenerResultsForTicker 100
                
            let view = renderStockInternal stock dateRangeScreenerHits lastNScreenerHits 

            let pageTitle = (stock.ticker |> StockTicker.value) + " - " + stock.company
            view |> mainLayout pageTitle
        | None -> 
            notFound $"Stock with {ticker} symbol not found"