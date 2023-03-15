namespace StockScreenerReports.Web.Handlers

module Dashboard =

    open Giraffe.ViewEngine
    open StockScreenerReports.Storage.Reports
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Core

    let private generateScreenerResultSection (screener:ScreenerResultReport) = 
        
        let screenerDate = screener.date |> Utils.convertToDateString
        
        div [_class "column is-one-quarter"] [
            
            Views.generateHrefWithAttr
                $"{screener.count}: {screener.name}"
                (Links.screenerResultsLink (screener.screenerid) screenerDate)
                (_class "button is-primary mr-2 is-fullwidth")
        ]

    let private generateJobStatusRow() =
        div [ _class "columns" ] [
            div [ _class "column" ] [ 
                ScreenerJob |> Views.genericJobStatusGet |> str 
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
            th [ _class "has-text-right" ] [ str "# of stocks" ]
        ]

        rows |> Views.fullWidthTableWithCssClass "dashboard-industry-table" header

    let private generateIndustryTrendsRow days =

        let columns =
            [
                Constants.NewHighsScreenerId, "New Highs"
                Constants.TopGainerScreenerId, "Top Gainers"
                Constants.TopLoserScreenerId, "Top Losers"
                Constants.NewLowsScreenerId, "New Lows"
            ]
            |> List.map (fun (screenerId, title) ->
                let industries = [screenerId] |> getTopIndustriesForScreeners days |> List.truncate 10
                div [ _class "column" ] [
                    generateTrendsTable title industries
                ]
            )

        [
            div [_class "columns"] columns
        ]

    let private generateSectorTrendsRow days =

        let gainers = Constants.NewHighsScreenerId |> getTopSectorsForScreener days
        let losers = Constants.NewLowsScreenerId |> getTopSectorsForScreener days

        [
            div [_class "columns"] [
                div [ _class "column" ] [
                    Views.toNameCountTableWithLinks "Sectors Trending Up" 5 Links.sectorLink gainers
                ]
                div [ _class "column" ] [
                    Views.toNameCountTableWithLinks "Sectors Trending Down" 5 Links.sectorLink losers
                ]
            ]
        ]

    let private generateSMATrendRows startDate endDate =

        let toDescription (sma:int) (trend:Trend) =
            $"<b>SMA {sma}:</b> {trend}"

        let toColor (sma:int) =
            match sma with
            | 20 -> Constants.ColorRed
            | 200 -> Constants.ColorBlue
            | _ -> Constants.ColorBlack

        let breakdowns =
            [20; 200]
            |> List.map (fun sma ->
                let smaBreakdown = sma |> getDailySMABreakdown startDate endDate
                (sma, smaBreakdown)
            )
    
        let datasets:list<Charts.DataSet<decimal>> =
            breakdowns
            |> List.map (fun (sma, smaBreakdown) ->
                {
                    data = smaBreakdown |> List.map (fun breakdown -> breakdown.percentAboveRounded)
                    title = $"SMA {sma}"
                    color = sma |> toColor
                }   
            )

        let labels = breakdowns.Head |> snd |> List.map (fun breakdown -> breakdown.date.ToString("MM/dd"))

        [
            div [_class "columns"]
                (
                    breakdowns
                    |> List.map (fun (sma, breakdown) ->
                        div [ _class "column" ] [
                            breakdown
                            |> TrendsCalculator.calculate
                            |> toDescription sma
                            |> rawText
                        ]
                    )
                )
            
            div [_class "block"]
                (Charts.generateChartElements "SMA breakdown" Charts.ChartType.Line (Some 100) Charts.smallChart labels datasets)
        ]

    let private createView (screeners:list<ScreenerResultReport>) =
        
        let (startDate, endDate) = FinvizConfig.dateRangeAsStrings

        let screenerRows =
            div [_class "columns is-multiline"] 
                (screeners |> List.map generateScreenerResultSection)

        let industryTrendRows = generateIndustryTrendsRow FinvizConfig.industryTrendDayRange
        let sectorTrendRows = generateSectorTrendsRow FinvizConfig.sectorTrendDayRange

        let smaTrendRows = generateSMATrendRows startDate endDate

        let jobStatusRow = generateJobStatusRow()

        [screenerRows] @ smaTrendRows @ industryTrendRows @ sectorTrendRows @ [ jobStatusRow ]

    let handler()  = 
        
        // get screeners, render them in HTML
        getLatestScreeners()
        |> createView
        |> Views.mainLayout "Dashboard"