namespace StockScreenerReports.Web.Handlers

module IndustryDashboard =
    open StockScreenerReports.Core
    open Giraffe.ViewEngine.Attributes
    open Giraffe.ViewEngine.HtmlElements
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Charts
    open StockScreenerReports.Web.Shared.Views
    open FSharp.Data
    open Giraffe
    open StockScreenerReports.Storage.Reports

    type IndustryExportType =   CsvProvider<
        Schema = "ticker, company, sector, industry, country",
        HasHeaders=false>

    let header = "ticker, company, sector, industry, country"

    let private createEarningsSection industryName fullDateRange = 
        
        // start date should be two weeks from the last date in the range
        // get the first item in tuple, then convert it from string to date, then subtract 14 days
        // and then convert it back to a string
        let startDate = fullDateRange |> snd |> Utils.convertToDateTime |> fun d -> d.AddDays(-14) |> Utils.convertToDateString
        let dateRange = (startDate, fullDateRange |> snd)

        let tickersWithEarnings = 
            getEarningsTickers dateRange
            |> List.map (fun (ticker,_) -> ticker)

        let stocksWithEarnings =
            tickersWithEarnings
            |> Storage.getStockByTickers
            |> List.filter (fun s -> s.industry = industryName)

        let newHighs =
            Constants.NewHighsScreenerId
            |> getScreenerResultsForDays dateRange
            |> List.map (fun s -> (s.ticker |> StockTicker.create, s))
            |> Map.ofList

        let topGainers = 
            Constants.TopGainerScreenerId
            |> getScreenerResultsForDays dateRange
            |> List.map (fun s -> (s.ticker |> StockTicker.create, s))
            |> Map.ofList

        let topLosers =
            Constants.TopLoserScreenerId
            |> getScreenerResultsForDays dateRange
            |> List.map (fun s -> (s.ticker |> StockTicker.create, s))
            |> Map.ofList

        let newLows =
            Constants.NewLowsScreenerId
            |> getScreenerResultsForDays dateRange
            |> List.map (fun s -> (s.ticker |> StockTicker.create, s))
            |> Map.ofList

        let earningsTableHeader = [ "Ticker"; "Company"; "New High"; "Top Gainer"; "Top Loser"; "New Low"; "Trading View" ]
        let earningsTable =
            stocksWithEarnings
            |> List.map (fun stock ->

                let generateDivWithDateAndIcon iconFunc (screenerResultOption:ScreenerResultReportItem option) =
                    match screenerResultOption with
                    | Some s ->
                        div [] [
                            true |> iconFunc
                            s.date |> Utils.convertToDateString |> str
                        ]
                    | None -> 
                        false |> iconFunc

                let newHighIcon = newHighs |> Map.tryFind stock.ticker |> generateDivWithDateAndIcon generateNewHighIcon
                let topGainerIcon = topGainers |> Map.tryFind stock.ticker |> generateDivWithDateAndIcon generateTopGainerIcon
                let topLoserIcon = topLosers |> Map.tryFind stock.ticker |> generateDivWithDateAndIcon generateTopLoserIcon
                let newLowIcon = newLows |> Map.tryFind stock.ticker |> generateDivWithDateAndIcon generateNewLowIcon

                [
                    TickerLinkColumn(stock.ticker |> StockTicker.value)
                    StringColumn(stock.company)
                    NodeColumn(newHighIcon)
                    NodeColumn(topGainerIcon)
                    NodeColumn(topLoserIcon)
                    NodeColumn(newLowIcon)
                    LinkNewTabColumn("chart", stock.ticker |> StockTicker.value |> Links.tradingViewLink)
                ] |> toTr
            )
            |> fullWidthTableWithSortableHeaderCells earningsTableHeader

        match stocksWithEarnings with
        | [] -> h4 [] ["No earnings last two weeks" |> str]
        | _ -> section [] [
            h4 [] [$"Earnings from {dateRange |> fst} to {dateRange |> snd}: {stocksWithEarnings.Length}" |> str]
            earningsTable
        ]

    let private createSMABreakdownSection industryName =
        // load industry trends
        let createBreakdownColumnDiv sma =
            let breakdown = industryName |> getMostRecentIndustrySMABreakdown sma

            let description =
                match breakdown with
                | None -> "No SMA breakdown found"
                | Some t -> 
                    let total = t.breakdown.above + t.breakdown.below
                    let pct = System.Math.Round((double t.breakdown.above) * 100.0 / (double total), 2)
                    $"<b>{pct}%%</b> ({t.breakdown.above} / {total}) above <b>{t.breakdown.days} SMA</b>"

            let hasTextRight = match sma with | Constants.SMA200 -> "has-text-right" | _ -> ""
            div [_class $"column {hasTextRight}"] [rawText description]
            
        section [] [
            h4 [] ["SMA Breakdown" |> str]
            div [_class "columns"] (
                Constants.SMAS
                |> List.map createBreakdownColumnDiv
            )
        ]

    let private createHeaderSection dateRange industryName  =

        let breakdowns = 
            industryName
            |> getIndustrySMABreakdownsForIndustry Constants.SMA20 dateRange
            
        let cycleScoreComponents =
            breakdowns
            |> MarketCycleScoring.cycleScoreComponents

        let cycleAddScore = 
            cycleScoreComponents
            |> MarketCycleScoring.componentScoreAdding

        let cycleMultiScore =
            cycleScoreComponents
            |> MarketCycleScoring.componentScoreMultiplying

        let trendScoreComponent =
            breakdowns
            |> MarketCycleScoring.trendScoreComponents

        let trendAddScore = 
            trendScoreComponent
            |> MarketCycleScoring.componentScoreAdding

        let trendMultiScore =
            trendScoreComponent
            |> MarketCycleScoring.componentScoreMultiplying

        let trendWithCycle =
            breakdowns
            |> TrendsCalculator.calculateTrendAndCycleForIndustry

        let trendCardClass =
            match trendAddScore with
            | x when x > 5 -> "card-positive"
            | _ -> "card-negative"

        let cycleCardClass =
            match cycleAddScore with
            | x when x > 5 -> "card-positive"
            | _ -> "card-negative"

        div [ _class "columns"] [
            div [ _class "column"] [
                h1 [] [ str industryName ]

                div [_class "card-container"] [
                    div [_class $"card {trendCardClass}"] [
                        div [_class "score-title"] [ "Trend" |> str ] 
                        div [_class "score-number"] [ $"{trendAddScore}/{trendMultiScore}" |> str ] 
                    ]
                    div [_class $"card {cycleCardClass}"] [
                        div [_class "score-title"] [ "Cycle" |> str ] 
                        div [_class "score-number"] [ $"{cycleAddScore}/{cycleMultiScore}" |> str ] 
                    ]
                ]
                
                div [ _class "mt-5"] [ trendWithCycle.cycle |> marketCycleToHtml |> rawText]
            ]
            div [ _class "column has-text-right"] [
                h5 [] [
                    span [ _class "mx-1"] [
                        industryName 
                        |> Links.industryFinvizLink
                        |> generateHrefNewTab "See it on Finviz"
                    ]
                    generateFilterSection dateRange
                ]
            ]
        ]

    let private smaBreakdownsAndSMACharts (dateRange) industryName =

        let createTrendDiv sma (trend:Option<IndustryTrend>) =
            let desc =
                match trend with
                | None -> "No trend found"
                | Some t -> t.trend |> trendToHtml

            let hasTextRight = match sma with | Constants.SMA200 -> "has-text-right" | _ -> ""

            div [ _class $"column {hasTextRight}"] [
                rawText desc
            ]

        let createDataset smaInterval  : DataSet<decimal> =
            let data =
                industryName
                |> getIndustrySMABreakdownsForIndustry smaInterval dateRange
                |> List.map (fun u -> System.Math.Round(u.breakdown.percentAbove, 0))
                
            {
                data = data
                title = $"{smaInterval} SMA Trend"
                color = smaInterval |> Constants.mapSmaToColor
            }

        let datasets = Constants.SMAS |> List.map createDataset

        let smoothedDataSets = datasets |> Utils.smoothedDataSets 3

        let labels = 
            industryName
            |> getIndustrySMABreakdownsForIndustry 20 dateRange
            |> List.map (fun u -> u.breakdown.date.ToString("MMM/dd"))
        
        let charts =
            datasets 
            |> generateChartElements "sma breakdown chart" Line (Some 100) smallChart labels

        let smoothedCharts =
            smoothedDataSets
            |> generateChartElements "sma breakdown chart" Line (Some 100) smallChart labels

        let trendDiv = div [_class "columns"] (
            Constants.SMAS
                |> List.map (fun sma -> 
                    industryName
                    |> getIndustryTrend sma (snd dateRange) 
                    |> createTrendDiv sma
                )
        )

        div [] [
            section [] [
                h4 [] ["SMA Trend Charts" |> str]
                trendDiv
                div [] charts
            ]
            section [] [
                h4 [] ["SMA Trend Windowed (3)" |> str]
                div [] smoothedCharts
            ]
        ]

    let handler industryName : HttpHandler =
        fun (next : HttpFunc) (ctx:Microsoft.AspNetCore.Http.HttpContext) ->
        
            let dateRange = getFilterSectionParams ctx

            let startDate = dateRange |> fst |> Utils.convertToDateTime
            let endDate = dateRange |> snd |> Utils.convertToDateTime

            // load charts for each screener
            let tradingDates = (startDate,endDate) |> ReportsConfig.listOfBusinessDates

            let labels = tradingDates |> Seq.map (fun u -> u.ToString("MMM/dd"))

            let screeners = Storage.getScreeners()
            let datasets = 
                screeners
                |> List.map (fun screener ->
                    let dailyCounts = getDailyCountsForScreenerAndIndustry screener.id industryName dateRange

                    let mapped = dailyCounts |> Map.ofList

                    let data =
                        tradingDates
                        |> Seq.map(fun (date) ->
                            let found = mapped.TryFind date
                            match found with
                            | Some c -> c
                            | None -> 0
                        )
                        |> Seq.toList
                    
                    {
                        data = data
                        title = screener.name
                        color = screener.id |> ReportsConfig.getBackgroundColorForScreenerId
                    }
                )

            let screenerChart = 
                section [] [
                    h4 [ _class "mt-4"] ["Screener Counts" |> str]
                    div [] (generateChartElements "screener chart" Bar None smallChart labels datasets)
                ]

            let resultRows =
                industryName
                |> getScreenerResultsForIndustry dateRange 50
                |> List.map (fun screenerResult ->
                    [
                        DateColumn(screenerResult.date)
                        NodeColumn((screenerResult.screenerid,screenerResult.screenername) |> generateScreenerTags)
                        TickerLinkColumn(screenerResult.ticker)
                        StringColumn(screenerResult.marketCap    |> marketCapFormatted)  
                        StringColumn(screenerResult.price        |> dollarFormatted)     
                        StringColumn(screenerResult.change       |> percentFormatted)    
                        StringColumn(screenerResult.volume       |> volumeFormatted)     
                        LinkNewTabColumn("chart", screenerResult.ticker |> Links.tradingViewLink)
                    ] |> toTr
                )

            let headerNames = [
                "Date";
                "screener";
                "Ticker";
                "Market Cap";
                "Price";
                "Change";
                "Volume";
                "Trading View"
            ]

            let screenerResultsTable =
                resultRows
                |> fullWidthTableWithSortableHeaderCells headerNames
            

            let stocks = industryName |> Storage.getStocksByIndustry
            let stockTable = stocks |> generateStockTable

            let tickers = stocks |> List.map (fun stock -> stock.ticker |> StockTicker.value)
            let stocksSection = section [_class "mt-5"] [
                h4 [] [
                    $"Stocks in Industry ({stocks.Length})" |> str
                    small [ _class "is-pulled-right"] [
                        generateHrefWithAttrs
                            "Export"
                            (industryName |> Links.industryExportLink)
                            [(_class "button is-small is-primary") ; (_target "_blank")]
                    ]
                    small [ _class "is-pulled-right mr-2"] [
                        generateHrefWithAttrs
                            "NGTD Outcomes"
                            ((industryName,tickers,[],"") |> Links.ngtdOutcomesReportLink)
                            [(_class "button is-small is-primary mr-2") ; (_target "_blank")]
                    ]
                ]
                stockTable
            ]
            
            let topLevel = [
                industryName |> createHeaderSection dateRange
                industryName |> createSMABreakdownSection
            ]

            let earningsSection = createEarningsSection industryName dateRange

            let smaBreakdownsAndChartSections = industryName |> smaBreakdownsAndSMACharts dateRange 

            let contentSections =
                [smaBreakdownsAndChartSections; screenerChart; earningsSection; stocksSection; screenerResultsTable]
                |> List.append topLevel

            let view = div [_class "content"] contentSections
            
            ([view] |> mainLayout $"{industryName} Industry Dashboard") next ctx

    let exportHandler industryName =
        setHttpHeader "Content-Type" "text/csv"
        >=> 
            let stocks = industryName |> Storage.getStocksByIndustry
            let filename = $"export_{industryName}.csv"
            let escapedFilename = System.Uri.EscapeDataString(filename)

            setHttpHeader "Content-Disposition" $"attachment; filename={escapedFilename}"
        >=>
            let rows = 
                stocks
                |> List.sortBy (fun s -> s.ticker)
                |> List.map (fun s -> 
                    IndustryExportType.Row(
                        s.ticker |> StockTicker.value,
                        s.company,
                        s.sector,
                        s.industry,
                        s.country
                    )
                )

            let csv = new IndustryExportType(rows)

            setBodyFromString (header + System.Environment.NewLine + csv.SaveToString())