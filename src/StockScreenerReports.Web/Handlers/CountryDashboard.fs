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
        screenerCountryResults
            |> List.map (fun screenerCountryResult ->
                let screener = screenerCountryResult.screener
                let data = screenerCountryResult.byDateHits
                
                data
                |> Charts.convertNameCountsToChart screener.name Charts.Bar None Charts.smallChart (ReportsConfig.getBackgroundColorForScreenerId screener.id)
                |> div [_class "block"] 
            )
            
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
        
        resultRows |> fullWidthTableWithSortableHeaderCells tableHeader

    let private renderHeader countryName =
        div [_class "content"] [
            h1 [] [
                str countryName
            ]
        ]
        
    let renderSMAChart dailySMABreakdowns =
        
        let datasets =
            dailySMABreakdowns
            |> List.map( fun (sma,breakdowns) ->
                let dataset : Charts.DataSet<decimal> =
                    let series = 
                        breakdowns
                        |> List.map (fun (u:CountrySMABreakdown) -> System.Math.Round(u.breakdown.percentAbove, 0))
                        
                    {
                        data = series
                        title = $"{sma} SMA Trend"
                        color = sma |> Constants.mapSmaToColor
                    }
                    
                dataset
                )
            
        let _, breakdowns = dailySMABreakdowns[0]

        let labels = breakdowns |> List.map (fun u -> u.breakdown.date.ToString("MMM/dd"))
            
        let chartElements =
            datasets |> Charts.generateChartElements "sma breakdown chart" Charts.ChartType.Line (Some 100) Charts.smallChart labels
        
        div [] chartElements
        
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
                    |> Seq.map(fun (date) ->
                        let found = mapped.TryFind date.Date
                        match found with
                        | Some c -> (date,c)
                        | None -> (date,0)
                    )
                
                {screener = screener; total = total; byDateHits = byDateHits;} 
            )
            
        let smaChart =
            Constants.SMAS
            |> List.map (fun sma -> sma, countryName)
            |> List.map (fun (sma, countryName) -> sma, countryName |> getCountrySMABreakdownsForCountry sma dateRangeAsStrings)
            |> renderSMAChart
        
        let charts = allScreenerResultsForCountry |> renderCharts
        let table = countryName |> getScreenerResultsForCountry 50  |> renderScreenerResultTable
        let header = countryName |> renderHeader

        [table] |> List.append charts |> List.append [smaChart] |> List.append [header] |> mainLayout $"Country Dashboard for {countryName}" 