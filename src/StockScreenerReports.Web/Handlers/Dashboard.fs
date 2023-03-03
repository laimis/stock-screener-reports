namespace StockScreenerReports.Web.Handlers

module Dashboard =

    open Giraffe.ViewEngine
    open StockScreenerReports.Storage.Reports
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Core

    let private generateScreenerResultSection (screener:ScreenerResultReport) = 
        
        let screenerDate = screener.date |> Utils.convertToDateString
        
        div [_class "content"] [
            h2 [] [
                
                Views.generateHrefWithAttr
                    $"{screener.count}"
                    (Links.screenerResultsLink (screener.screenerid) screenerDate)
                    (_class "button is-primary mr-2")

                str screener.name
            ]
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
                    td [] [ 
                        Views.generateHref
                            name
                            (Links.industryLink name)
                    ]
                    td [ _class "has-text-right"] [ str (count.ToString()) ]
                ])

        let header = tr [] [
            th [] [ str title ]
            th [ _class "has-text-right" ] [ str "# of stocks" ]
        ]

        rows |> Views.fullWidthTable header

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

        let sma20 = 20 |> getDailySMABreakdown startDate endDate
        let sma200 = 200 |> getDailySMABreakdown startDate endDate

        let trend20 = TrendsCalculator.calculate sma20
        let trend200 = TrendsCalculator.calculate sma200

        let toDescription (sma:int) (trend:Trend) =
            $"<b>SMA {sma}:</b> {trend}"

        let sma20DirectionDescription = trend20 |> toDescription 20
        let sma200DirectionDescription = trend200 |> toDescription 200

        let datasets:list<Charts.DataSet<decimal>> = [
            {
                data = sma20 |> List.map (fun breakdown -> breakdown.percentAboveRounded)
                title = "SMA 20"
                color = Constants.ColorRed
            };
            {
                data = sma200 |> List.map (fun breakdown -> breakdown.percentAboveRounded)
                title = "SMA 200"
                color = Constants.ColorBlue
            }
        ]

        let labels = sma20 |> List.map (fun breakdown -> breakdown.date.ToString("MM/dd"))

        [
            div [_class "columns"] [
                div [ _class "column" ] [
                    rawText sma20DirectionDescription
                ]
                div [ _class "column" ] [
                    rawText sma200DirectionDescription
                ]
            ]
            div [_class "block"]
                (Charts.generateChartElements "SMA breakdown" Charts.ChartType.Line (Some 100) Charts.smallChart labels datasets)
        ]

    let private createView (screeners:list<ScreenerResultReport>) =
        
        let (startDate, endDate) = FinvizConfig.dateRangeAsStrings

        let screenerRows = screeners |> List.map generateScreenerResultSection

        let industryTrendRows = generateIndustryTrendsRow FinvizConfig.industryTrendDayRange
        let sectorTrendRows = generateSectorTrendsRow FinvizConfig.sectorTrendDayRange

        let smaTrendRows = generateSMATrendRows startDate endDate

        let jobStatusRow = generateJobStatusRow()

        screenerRows @ smaTrendRows @ industryTrendRows @ sectorTrendRows @ [ jobStatusRow ]

    let handler()  = 
        
        // get screeners, render them in HTML
        getLatestScreeners()
        |> createView
        |> Views.mainLayout "Dashboard"