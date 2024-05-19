namespace StockScreenerReports.Web.Handlers

open StockScreenerReports.Storage.Reports

module CountryDashboard =
    open StockScreenerReports.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open StockScreenerReports.Storage
    open Giraffe.ViewEngine.Attributes
    open StockScreenerReports.Core
    open StockScreenerReports.Web.Shared.Views
    
    type private ScreenerCountryResult = {
        screener: Screener
        total: int
        byDateHits: seq<System.DateTime * int>
    }
    
    let private renderCharts screenerCountryResults =
        
        let labels = screenerCountryResults |> List.head |> fun x -> x.byDateHits |> Seq.map (fun (date,_) -> date |> Utils.formatDateForChart) |> Seq.toList
        
        let datasets:list<Charts.DataSet<int>> = 
            screenerCountryResults
            |> List.map( fun result ->
                let data = result.byDateHits |> Seq.map (fun (_,count) -> count) |> Seq.toList
                
                {
                    data = data
                    title = result.screener.name
                    color = result.screener.id |> ReportsConfig.getBackgroundColorForScreenerId
                }
            )
        
        div [] (Charts.generateChartElements "Screener hits" Charts.Bar (Some 1) Charts.smallChart labels datasets)
        |> toSection "Screener Hits Chart"
            
    let private renderScreenerResultTable (screenerResults:ScreenerResultReportItem list) =
        let resultRows =
            screenerResults
            |> List.map (fun screenerResult ->
                [
                    DateColumn(screenerResult.date)
                    NodeColumn((screenerResult.screenerid,screenerResult.screenername) |> generateScreenerTags)
                    TickerLinkColumn(screenerResult.ticker)
                    StringColumn(screenerResult.marketCap |> marketCapFormatted)
                    StringColumn(screenerResult.price |> dollarFormatted)
                    StringColumn(screenerResult.change |> percentFormatted)
                    StringColumn(screenerResult.volume |> volumeFormatted)
                    LinkColumn("chart", screenerResult.ticker |> Links.tradingViewLink)
                ] |> toTr
            )

        let tableHeader = [
            "Date"; "Screener"; "Ticker"; "Market Cap"; "Price"; "Change"; "Volume"; "Trading View"
        ]
        
        resultRows |> fullWidthTableWithSortableHeaderCells tableHeader |> toSection "Screener Hits"

    let private renderHeader countryName =
        div [_class "columns content"] [
            div [_class "column"] [
                h1 [] [
                    str countryName
                ]
            ]
            div [ _class "column has-text-right"] [
                h5 [] [
                    span [ _class "mx-1"] [
                        countryName 
                        |> Links.countryFinvizLink
                        |> generateHrefNewTab "See it on Finviz"
                    ]
                ]
            ]
        ]
        
    let renderSMAChart dailySMABreakdowns =
        
        let datasets =
            dailySMABreakdowns
            |> List.map( fun (sma:SMA,breakdowns) ->
                let dataset : Charts.DataSet<decimal> =
                    let series = 
                        breakdowns
                        |> List.map (fun (u:CountrySMABreakdown) -> System.Math.Round(u.breakdown.percentAbove, 0))
                        
                    {
                        data = series
                        title = $"{sma.Interval} SMA Trend"
                        color = sma.Color
                    }
                    
                dataset
            )
            
        let _, breakdowns = dailySMABreakdowns[0]

        let labels = breakdowns |> List.map (fun u -> u.breakdown.date |> Utils.formatDateForChart)
            
        let chartElements =
            datasets |> Charts.generateChartElements "sma breakdown chart" Charts.ChartType.Line (Some 100) Charts.smallChart labels
        
        div [] chartElements |> toSection "SMA Breakdown"
        
    let handler countryName =
        let dateRangeAsStrings = ReportsConfig.dateRangeAsStrings()

        let businessDates = ReportsConfig.dateRange() |> ReportsConfig.listOfBusinessDates
        
        let allScreenerResultsForCountry =
            Storage.getScreeners()
            |> List.map (fun screener ->
                let data = getDailyCountsForScreenerAndCountry screener.id countryName dateRangeAsStrings
                let mapped = data |> Map.ofList
                let total = mapped |> Map.fold (fun acc _ v -> acc + v) 0
                let byDateHits =
                    businessDates
                    |> Seq.map(fun date ->
                        let found = mapped.TryFind date.Date
                        match found with
                        | Some c -> date,c
                        | None -> date,0
                    )
                
                {screener = screener; total = total; byDateHits = byDateHits;} 
            )
            
        let countryStocks = countryName |> Storage.getStocksByCountry
        
        // render all data now
        
        let smaChart =
            SMA.All
            |> List.map (fun sma -> sma, countryName)
            |> List.map (fun (sma, countryName) -> sma, countryName |> getCountrySMABreakdownsForCountry sma dateRangeAsStrings)
            |> renderSMAChart
        
        let countryStocksTable = countryStocks |> generateStockTable
        let screenerHitsChart = allScreenerResultsForCountry |> renderCharts
        let screenerHitsTable = countryName |> getScreenerResultsForCountry 50  |> renderScreenerResultTable
        let header = countryName |> renderHeader
        
        [header; smaChart; countryStocksTable; screenerHitsChart; screenerHitsTable] |> mainLayout $"Country Dashboard for {countryName}" 