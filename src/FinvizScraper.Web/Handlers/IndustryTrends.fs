namespace FinvizScraper.Web.Handlers

module IndustryTrends =
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
                        td [] [ update.total.ToString() |> str ]
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
                ]

                let cells = List.append (List.append commonCells sma20Cells) sma200Cells

                tr [] cells
            )

        let industry20And200Header = tr [] [
            th [] [ str "Industry" ]
            th [ _colspan "3"] [ str "20" ]
            th [ _colspan "3"] [ str "200" ]
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

        let view = [
            div [_class "content"] [
                h1 [] [str "Industry Trends"]
            ]
            h3 [] [ str "20/200 SMA breakdown"]
            industryTrendTable
            jobStatusRow
        ]
        
        view |> mainLayout $"Industry Trends" 