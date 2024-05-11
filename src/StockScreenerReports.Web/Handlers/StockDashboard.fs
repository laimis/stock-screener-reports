namespace StockScreenerReports.Web.Handlers

module StockDashboard =
    open Giraffe
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
        
    let createCorporateActionsSection corporateActions =
        
        let corporateActionToRow (corporateAction:CorporateAction) =
            [
                StringColumn(corporateAction.Date)
                StringColumn(corporateAction.Type)
                StringColumn(corporateAction.Action)
            ] |> toTr

        let allCorporateActionsRows = corporateActions |> List.map corporateActionToRow

        let tableHeader = [
            "Date"
            "Type"
            "Action"
        ]

        match allCorporateActionsRows with
        | [] ->
            div [] [ str "No corporate actions found for this stock" ]
        | _ ->
            allCorporateActionsRows |> fullWidthTableWithSortableHeaderCells tableHeader |> toSection "Corporate Actions"

    let createScreenerResultsSection results =
        let screenerResultToRow (screenerResult:ScreenerResultReportItem) =
            let screenerLink = screenerResult.date |> screenerResultsLink screenerResult.screenerid

            [
                LinkColumn(screenerResult.date |> Utils.convertToDateString, screenerLink)
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

        match allScreenerResultsRows with
        | [] ->
            div [] [ str "No screener results found for this stock" ]
        | _ ->
            allScreenerResultsRows |> fullWidthTableWithSortableHeaderCells tableHeader |> toSection "All Screener Results"

    let createScreenerHitChart tickerScreenerResults =
        let dateRange = ReportsConfig.dateRange()

        let businessDays = ReportsConfig.listOfBusinessDates dateRange

        // group recent screenerresults by date
        let recentScreenerResultsByScreener = 
            tickerScreenerResults
            |> List.groupBy (fun screenerResult -> (screenerResult.screenerid, screenerResult.screenername))
            |> Map.ofList
            
        let labels = businessDays |> Seq.map (fun date -> date |> Utils.convertToDateString)

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

        match tickerScreenerResults with
        | [] ->
            div [] [ str "No screener hits found for this stock" ]
        | _ ->
            div [] (Charts.generateChartElements "Screener hits" Charts.Bar (Some 1) Charts.smallChart labels datasets)
            |> toSection "Screener Hits"
        


    let private renderStockInternal (stock:Stock) dateRangeScreenerHits lastNScreenerHits corporateActions =

        let header = stock |> createHeader

        let screenerHitChart = dateRangeScreenerHits |> createScreenerHitChart 

        let screenerResultsSection = lastNScreenerHits |> createScreenerResultsSection
        
        let corporateActionsSection = corporateActions |> createCorporateActionsSection 

        [
            header
            screenerHitChart
            screenerResultsSection
            corporateActionsSection
        ]

    let handler ticker : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) -> task {
            let stock =
                ticker
                |> StockTicker.create
                |> Storage.getStockByTicker

            match stock with
            | Some stock ->
                let dateRangeScreenerHits = ReportsConfig.dateRangeAsStrings() |> getScreenerResultsForTickerDayRange stock.ticker
                let lastNScreenerHits = stock.ticker |> getScreenerResultsForTicker 100
                let! corporateActions = stock.ticker |> Storage.getCorporateActionsForTicker
                let view = renderStockInternal stock dateRangeScreenerHits lastNScreenerHits corporateActions
                let pageTitle = (stock.ticker |> StockTicker.value) + " - " + stock.company
                return! (view |> mainLayout pageTitle) next ctx
            | None -> 
                return! (notFound $"Stock with {ticker} symbol not found") next ctx
        }