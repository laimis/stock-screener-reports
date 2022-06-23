namespace FinvizScraper.Web.Handlers

module IndustriesDashboard =
    open FinvizScraper.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open FinvizScraper.Storage
    open FinvizScraper.Web.Shared.Views
    open Giraffe.ViewEngine

    let private generateIndustry20And200Table() =
        let date = Storage.getIndustryUpdatesLatestDate() |> FinvizScraper.Core.FinvizConfig.formatRunDate

        let getIndustryUpdatesAndTurnToMap (days:int) =
            date 
            |> Storage.getIndustryUpdates days
            |> List.map (fun x -> (x.industry, x))
            |> Map.ofList
        
        let industryUpdates20 = getIndustryUpdatesAndTurnToMap 20
        let industryUpdates200 = getIndustryUpdatesAndTurnToMap 200

        let industry20And200Rows =
            industryUpdates200
            |> Map.toList
            |> List.sortByDescending (fun (key, update200) -> 
                let update20 = industryUpdates20[key]
                (update200.percentAbove, update20.percentAbove)
            )
            |> List.map (fun (key, iu) ->

                let toSMACells (update:FinvizScraper.Core.IndustryUpdate) =
                    [
                        td [] [ update.above.ToString() |> str  ]
                        td [] [ System.String.Format("{0:N2}%", update.percentAbove) |> str ]
                    ]

                let sma20Cells = toSMACells (industryUpdates20[key])
                let sma200Cells = toSMACells (iu)

                let commonCells = [
                    td [] [ 
                        span [ _class "mr-2"] [
                            iu.industry |> Links.industryLink |> generateHref iu.industry
                        ]
                        
                        span [ _class "is-pulled-right"] [
                            iu.industry |> Links.industryFinvizLink |> generateHrefNewTab "finviz" 
                        ]
                    ]
                    td [] [(iu.total).ToString() |> str]
                ]

                let cells = List.append (List.append commonCells sma20Cells) sma200Cells

                tr [] cells
            )

        let industry20And200Header = tr [] [
            th [] [ str "Industry" ]
            th [] [ str "# Stocks" ]
            th [] [ str "20 sma" ]
            th [] [ str "20 sma %" ]
            th [] [ str "200 sma" ]
            th [] [ str "200 sma %" ]
        ]

        industry20And200Header::industry20And200Rows |> fullWidthTable

    let handler()  =
        
        let industryTrendTable = generateIndustry20And200Table()

        let jobStatusRow =
            div [ _class "columns" ] [
                div [ _class "column" ] [ 
                    FinvizScraper.Core.IndustryTrendsJob |> genericJobStatusGet |> str 
                ]
            ]

        let (above20, below20) = Reports.getStockSMABreakdown 20
        let (above200, below200) = Reports.getStockSMABreakdown 200

        let above20percent = System.Math.Round((float above20 * 100.0) / (float above20 + float below20), 2).ToString() + "%"
        let above200percent = System.Math.Round((float above200 * 100.0) / (float above200 + float below200), 2).ToString() + "%"

        let totals =
            div [ _class "columns" ] [
                div [ _class "column" ] [
                    b [] [ str "20 SMA" ]
                    str $": {above20}/{above20 + below20}, "
                    b [] [ str above20percent]
                ]
                div [ _class "column" ] [
                    b [] [ str "200 SMA" ]
                    str $": {above200}/{above200 + below200}, "
                    b [] [ str above200percent]
                ]
            ]

        let view = [
            div [_class "content"] [
                h1 [] [str "Industries"]
            ]
            totals
            industryTrendTable
            jobStatusRow
        ]
        
        view |> mainLayout $"Industry Trends" 