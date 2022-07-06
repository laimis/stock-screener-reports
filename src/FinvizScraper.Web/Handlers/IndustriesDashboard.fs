namespace FinvizScraper.Web.Handlers

module IndustriesDashboard =
    open Giraffe
    open FinvizScraper.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open FinvizScraper.Storage
    open FinvizScraper.Web.Shared.Views
    open Giraffe.ViewEngine
    open FinvizScraper.Core
    

    let private generateIndustry20And200Table() =
        let latestDate = Reports.getIndustrySMABreakdownLatestDate()
        let formattedDate = latestDate |> FinvizConfig.formatRunDate

        let thirtyDaysAgo = Utils.addDaysToClosestBusinessDay latestDate -30 |> FinvizConfig.formatRunDate
        let sixtyDaysAgo = Utils.addDaysToClosestBusinessDay latestDate -60 |> FinvizConfig.formatRunDate

        let getIndustrySMABreakdownsAndTurnToMap date (days:int) =
            date 
            |> Reports.getIndustrySMABreakdowns days
            |> List.map (fun x -> (x.industry, x))
            |> Map.ofList
        
        let industrySMABreakdowns20 = getIndustrySMABreakdownsAndTurnToMap formattedDate 20
        let industrySMABreakdown200 = getIndustrySMABreakdownsAndTurnToMap formattedDate 200
        let smaBreakdown200_30days = getIndustrySMABreakdownsAndTurnToMap thirtyDaysAgo 200
        let smaBreakdown200_60days = getIndustrySMABreakdownsAndTurnToMap sixtyDaysAgo 200

        let getIndustryTrendsANdTurnToMap (days:int) =
            days
            |> Reports.getIndustryTrends
            |> List.map (fun x -> (x.industry, x))
            |> Map.ofList

        let industryTrend20 = getIndustryTrendsANdTurnToMap 20
        let industryTrend200 = getIndustryTrendsANdTurnToMap 200

        let mutable counter = 0

        let industry20And200Rows =
            industrySMABreakdown200
            |> Map.toList
            |> List.sortByDescending (fun (key, update200) -> 
                let update20 = industrySMABreakdowns20[key]
                (update200.breakdown.percentAbove, update20.breakdown.percentAbove)
            )
            |> List.map (fun (key, iu) ->

                let toSMACells (update:FinvizScraper.Core.IndustrySMABreakdown) trend =
                    [
                        td [] [ $"{update.breakdown.above} / {update.breakdown.total}" |> str  ]
                        td [] [ System.String.Format("{0:N2}%", update.breakdown.percentAbove) |> str ]
                        td [] [ System.String.Format("{0:N0}", trend.change) |> str ]
                        td [] [ System.String.Format("{0:N0}", trend.streak) |> str ]
                    ]

                counter <- counter + 1

                let sma20Cells = toSMACells (industrySMABreakdowns20[key]) (industryTrend20[key])
                let sma200Cells = toSMACells (iu) (industryTrend200[key])

                let breakdownDiff breakdownMap =
                    let toCompare =
                        match (breakdownMap |> Map.tryFind key) with
                        | Some update -> update
                        | None -> iu

                    let diff = iu.breakdown.percentAbove - toCompare.breakdown.percentAbove
                    
                    td [] [ System.Math.Round(diff, 0).ToString() |> str ]

                let commonCells = [
                    td [] [ counter.ToString() |> str ]
                    td [] [ 
                        span [ _class "mr-2"] [
                            iu.industry |> Links.industryLink |> generateHref iu.industry
                        ]
                        
                        span [ _class "is-pulled-right"] [
                            iu.industry |> Links.industryFinvizLink |> generateHrefNewTab "finviz" 
                        ]
                    ]
                ]

                let diffCells = [
                    breakdownDiff smaBreakdown200_30days
                    breakdownDiff smaBreakdown200_60days
                ]

                let cells = commonCells @ sma20Cells @ sma200Cells @ diffCells

                tr [] cells
            )

        let industry20And200Header = tr [] [
            th [] []
            toSortableHeaderCell "Industry"
            toSortableHeaderCell "20 sma"
            toSortableHeaderCell "20 sma %"
            toSortableHeaderCell "Trend Change"
            toSortableHeaderCell "Trend Streak"
            toSortableHeaderCell "200 sma"
            toSortableHeaderCell "200 sma %"
            toSortableHeaderCell "Trend Change"
            toSortableHeaderCell "Trend Streak"
            toSortableHeaderCell "30 diff"
            toSortableHeaderCell "60 diff"
        ]

        industry20And200Header::industry20And200Rows |> fullWidthTable

    let handler : HttpHandler  =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->

            let industriesTable = generateIndustry20And200Table()

            let jobStatusRow =
                div [ _class "columns" ] [
                    div [ _class "column" ] [ 
                        IndustryTrendsJob |> genericJobStatusGet |> str 
                    ]
                ]

            let view = [
                div [_class "content"] [
                    h1 [] [str "Industries"]
                ]
                industriesTable
                jobStatusRow
            ]
            
            (view |> mainLayout $"Industries") next ctx