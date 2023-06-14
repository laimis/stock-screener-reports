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
                _href (Links.screenerResultsLink (screener.screenerid) screenerDate)] [
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

    let private generateIndustryTrendsRow data =

        let columns =
            data
            |> List.map (fun (title, industries) ->
                div [ _class "column" ] [
                    industries |> List.truncate 10 |> generateTrendsTable title
                ]
            )

        [
            section [ _class "content" ] [
                h4 [] [ str "Industry Trends" ]
            ]
            div [_class "columns"] columns
        ]

    let private generateSectorTrendsRow data =

        let columns = 
            
            data |> List.map( fun (title, data) ->
                div [ _class "column" ] [
                    Views.toNameCountTableWithLinks title 5 Links.sectorLink data
                ]
            )

        [
            section [ _class "content" ] [
                h4 [] [ str "Sector Trends" ]
            ]
            div [_class "columns"] columns
        ]

    let private generateSMATrendRows smaTrendCyclePairs =
        let mapTrendToHtml (sma:int) (trend:Trend) =
            $"SMA <b>{sma}</b>: {trend |> Views.trendToHtml}"

        let mapMarketCycleToHtml (cycle:MarketCycle) =
            $"Market cycle: {cycle |> Views.marketCycleToHtml}"

        [
            section [ _class "content" ] [
                h4 [] [ str "Market Trends" ]
            ]
            div [_class "columns"]
                (
                    smaTrendCyclePairs
                    |> List.map (fun (sma, trendWithCycle) ->
                        let hasTextRight = match sma with | Constants.SMA200 -> "has-text-right" | _ -> ""

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
                )
        ]

    let private generateSMABreakdownRows breakdowns =

        let datasets:list<Charts.DataSet<decimal>> =
            breakdowns
            |> List.map (fun (sma, (smaBreakdown:list<SMABreakdown>)) ->
                {
                    data = smaBreakdown |> List.map (fun breakdown -> breakdown.percentAboveRounded)
                    title = $"SMA {sma}"
                    color = sma |> Constants.mapSmaToColor
                }   
            )

        let smoothedDataSets = datasets |> Utils.smoothedDataSets 3

        let labels = breakdowns.Head |> snd |> List.map (fun breakdown -> breakdown.date.ToString("MM/dd"))
        let charts = datasets |> Charts.generateChartElements "SMA breakdown" Charts.ChartType.Line (Some 100) Charts.smallChart labels
        let smoothedCharts = smoothedDataSets |> Charts.generateChartElements "SMA breakdown (smoothed)" Charts.ChartType.Line (Some 100) Charts.smallChart labels

        [
            section [_class "content"] [
                h4 [] [ str "SMA Breakdown" ]
                div [_class "block"] charts
            ]
            section [ _class "content" ] [
                h4 [] [ str "SMA Breakdown (smoothed)" ]
                div [_class "block"] smoothedCharts
            ]
        ]

    let private createView () =
        
        let screeners = getLatestScreeners()
        
        let screenerRows =
            div [_class "columns is-multiline"] 
                ((screeners |> List.map generateScreenerResultSection) @ [
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

        let industryTrendRows = generateIndustryTrendsRow industryTrendData
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
        let sectorTrendRows = generateSectorTrendsRow sectorTrendData
        // end sector trends

        let breakdowns =
            Constants.SMAS
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
        let smaBreakdownRows = generateSMABreakdownRows breakdowns
        // SMA breakdown charts

        // cycle starts
        let marketCycleSection =
            Constants.SMA20
            |> Storage.getIndustryCycles
            |> Cycles.generateIndustryCycleStartChart

        [screenerRows] @ smaTrendRows @ smaBreakdownRows @ [marketCycleSection] @ industryTrendRows @ sectorTrendRows

    let handler()  = 
        
        // get screeners, render them in HTML
        createView()
        |> Views.mainLayout "Dashboard"