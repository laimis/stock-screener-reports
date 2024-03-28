namespace StockScreenerReports.Web.Handlers

module Dashboard =

    open Giraffe.ViewEngine
    open StockScreenerReports.Storage.Reports
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Core
    open StockScreenerReports.Storage

    let private generateRefreshButton() =
        let link = Links.jobsScreeners
        let title = "Refresh"
        div [_class "column is-one-quarter"] [
            a [
                _class "button is-light is-fullwidth"
                _style "justify-content: left;"
                _href link] [
                title |> str
            ]
        ]
    let private generateScreenerResultSection (screener:ScreenerResultReport) = 
        
        let screenerDate = screener.date |> Utils.convertToDateString
        
        div [_class "column is-one-quarter"] [
            a [
                _class "button is-primary is-fullwidth"
                _style "justify-content: left;"
                _href (Links.screenerResultsLink screener.screenerid screenerDate)] [
                span [
                    _style "font-size: 1.5em; font-weight: bold; padding-right: 10px"
                ] [
                    screener.count.ToString() |> str
                ]
                $"{screener.name}" |> str
            ]
        ]

    let generateTrendsTable title nameCounts =
        let rows =
            nameCounts
            |> List.truncate 10
            |> List.map (fun (name,count) ->

                tr [] [
                    td [
                        _onclick $"highlightDashboardIndustry(event)"
                    ] [
                        (Links.industryLink name) |> Views.generateHref name
                    ]
                    td [ 
                        _class "has-text-right"
                    ] [
                        str (count.ToString())
                    ]
                ])

        let header = tr [] [
            th [] [ str title ]
            th [ _class "has-text-right" ] [ str "" ]
        ]

        rows |> Views.fullWidthTableWithCssClass "dashboard-industry-table" header

    let private generateIndustryTrendsSection data =

        let columns =
            div [_class "columns"] 
                (
                    data
                    |> List.map (fun (title, industries) ->
                        div [ _class "column" ] [
                            industries |> List.truncate 10 |> generateTrendsTable title
                        ]
                    )
                )

        columns |> Views.toSection "Industry Trends"
        

    let private generateSectorTrendsSection data =

        let columns = 
            
            data |> List.map( fun (title, data) ->
                div [ _class "column" ] [
                    Views.toNameCountTableWithLinks title 5 Links.sectorLink Views.SectorClicked data
                ]
            )
        
        div [_class "columns"] columns |> Views.toSection "Sector Trends"

    let private generateSMATrendRows smaTrendCyclePairs =
        let mapTrendToHtml (sma:SMA) (trend:Trend) =
            $"SMA <b>{sma.Interval}</b>: {trend |> Views.trendToHtml}"

        let mapMarketCycleToHtml (cycle:MarketCycle) =
            $"{cycle |> Views.marketCycleToHtml}"

        let columns =
            smaTrendCyclePairs
                    |> List.map (fun (sma, trendWithCycle) ->
                        let hasTextRight = match sma with | SMA200 -> "has-text-right" | _ -> ""

                        let trend = trendWithCycle.trend
                        let cycle = trendWithCycle.cycle

                        let trendDiv = div [] [
                            trend |> mapTrendToHtml sma |> rawText
                        ]

                        let cycleDiv = div [] [
                            cycle |> mapMarketCycleToHtml |> rawText
                        ]
                            
                        div [ _class $"column {hasTextRight}" ] [
                            trendDiv
                            cycleDiv
                        ]
                    )
        
        div [_class "columns"] columns |> Views.toSection "Market and SMA Trends"

    let private generateSMABreakdownRows breakdowns =

        let datasets:list<Charts.DataSet<decimal>> =
            breakdowns
            |> List.map (fun (sma:SMA, smaBreakdown:list<SMABreakdown>) ->
                {
                    data = smaBreakdown |> List.map (fun breakdown -> breakdown.percentAboveRounded)
                    title = $"SMA {sma}"
                    color = sma.Color
                }   
            )

        let smoothedDataSets = datasets |> Utils.smoothedDataSets 3

        let labels = breakdowns.Head |> snd |> List.map (fun breakdown -> breakdown.date.ToString("MM/dd"))
        let charts = datasets |> Charts.generateChartElements "SMA breakdown" Charts.ChartType.Line (Some 100) Charts.smallChart labels
        let smoothedCharts = smoothedDataSets |> Charts.generateChartElements "SMA breakdown (smoothed)" Charts.ChartType.Line (Some 100) Charts.smallChart labels

        (
            div [_class "block"] charts |> Views.toSection "SMA Breakdown",
            div [_class "block"] smoothedCharts |> Views.toSection "SMA Breakdown (smoothed)"
        )

    let private createView () =
        
        let screenerResults = getLatestScreenerResults()
    
        let screenerRows =
            div [_class "columns is-multiline"] 
                ((screenerResults |> List.map generateScreenerResultSection) @ [
                    generateRefreshButton()
                ])
        
        // industry trends
        let industryTrendData =
            [
                Constants.NewHighsScreenerId, "New Highs"
                Constants.TopGainerScreenerId, "Top Gainers"
                Constants.TopLoserScreenerId, "Top Losers"
                Constants.NewLowsScreenerId, "New Lows"
            ] |> List.map (fun (screenerId, title) ->
                let industries =
                    [screenerId]
                    |> getTopIndustriesForScreeners ReportsConfig.industryTrendDayRange
                (title, industries)
            )

        let industryTrendRows = generateIndustryTrendsSection industryTrendData
        // end industry trends

        // sector trends
        let sectorTrendData =
            [
                (Constants.NewHighsScreenerId, "Sectors Trending Up")
                (Constants.NewLowsScreenerId, "Sectors Trending Down")
            ]
            |> List.map ( fun (screenerId, title) ->
                (title, (getTopSectorsForScreener ReportsConfig.sectorTrendDayRange screenerId))
            )
        let sectorTrendRows = generateSectorTrendsSection sectorTrendData
        // end sector trends

        let breakdowns =
            SMA.All
            |> List.map (fun sma ->
                let smaBreakdown = sma |> getDailySMABreakdown (ReportsConfig.dateRangeAsStrings())
                (sma, smaBreakdown)
            )

        // trend rows
        let smaTrendCyclePairs =
            breakdowns
            |> List.map (fun (sma, smaBreakdown) ->
                let trendWithCycle = smaBreakdown |> TrendsCalculator.calculate
                (sma, trendWithCycle)
            )
        let smaTrendRows = generateSMATrendRows smaTrendCyclePairs    
        // trend rows
        
        // SMA breakdown charts
        let smaBreakdownChartSection, smaBreakdownSmoothedChartSection = generateSMABreakdownRows breakdowns
        // SMA breakdown charts

        // cycle starts
        let marketCycleSection =
            SMA20
            |> Storage.getIndustryCycles
            |> Cycles.generateIndustryCycleStartChart

        // get latest job runs
        let missedJobs =
            Storage.getJobs()
            |> List.filter (fun job -> job.name = EarningsJob || job.name = ScreenerJob || job.name = TrendsJob)
            |> List.filter Utils.failedJobFilter

        let warningSection = Views.jobAlertSection missedJobs

        let time = ReportsConfig.now()
        let timeRow = div [_class "columns"] [
            str $"Last updated: {time}"
        ]

        [
            warningSection
            screenerRows
            smaTrendRows
            smaBreakdownChartSection
            smaBreakdownSmoothedChartSection
            marketCycleSection
            industryTrendRows
            sectorTrendRows
            timeRow
        ]

    let handler()  = 
        
        // get screeners, render them in HTML
        createView()
        |> Views.mainLayout "Dashboard"