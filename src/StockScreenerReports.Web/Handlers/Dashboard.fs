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

    let generateTrendsTable title nameCounts (industrySMABreakdowns:list<IndustrySMABreakdown>) =
        let rows =
            nameCounts
            |> List.truncate 10
            |> List.map (fun (name,count) ->

                let industryRankOption = 
                    industrySMABreakdowns |> List.tryFindIndex (fun (industry) ->
                        industry.industry = name
                    )

                let industryRank =
                    match industryRankOption with
                    | Some index ->
                        (index + 1).ToString()
                    | None -> "N/A"

                tr [] [
                    td [] [ 
                        Views.generateHref
                            name
                            (Links.industryLink name)
                    ]
                    td [ _class "has-text-right" ] [
                        str industryRank
                    ]
                    td [ _class "has-text-right"] [ str (count.ToString()) ]
                ])

        let header = tr [] [
            th [] [ str title ]
            th [ _class "has-text-right" ] [ str "Industry Rank" ]
            th [ _class "has-text-right" ] [ str "# of stocks" ]
        ]

        header::rows |> Views.fullWidthTable

    let private generateIndustryTrendsRow days =

        let gainers = [Constants.NewHighsScreenerId] |> getTopIndustriesForScreeners days |> List.take 5
        let losers = [Constants.NewLowsScreenerId] |> getTopIndustriesForScreeners days |> List.take 5

        let industrySMABreakdowns = 
            getIndustrySMABreakdownLatestDate()
            |> Utils.convertToDateString
            |> getIndustrySMABreakdowns 200

        [
            div [_class "columns"] [
                div [ _class "column" ] [
                    generateTrendsTable "Industries with New Highs" gainers industrySMABreakdowns
                ]
                div [ _class "column" ] [
                    generateTrendsTable "Industries with New Lows" losers industrySMABreakdowns
                ]
            ]
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

    let private generateSMATrendRows() =

        let numberOfDays = 40
        let sma20 = getDailySMABreakdown 20 numberOfDays
        let sma200 = getDailySMABreakdown 200 numberOfDays

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
            div [_class "block"]
                (Charts.generateChartElements "SMA breakdown" Charts.ChartType.Line (Some 100) Charts.smallChart labels datasets)
        ]

    let private createView (screeners:list<ScreenerResultReport>) =
        
        let screenerRows = screeners |> List.map generateScreenerResultSection

        let industryTrendRows = generateIndustryTrendsRow FinvizConfig.industryTrendDayRange
        let sectorTrendRows = generateSectorTrendsRow FinvizConfig.sectorTrendDayRange

        let smaTrendRows = generateSMATrendRows()

        let jobStatusRow = generateJobStatusRow()

        screenerRows @ smaTrendRows @ industryTrendRows @ sectorTrendRows @ [ jobStatusRow ]

    let handler()  = 
        
        // get screeners, render them in HTML
        getLatestScreeners()
        |> createView
        |> Views.mainLayout "Dashboard"