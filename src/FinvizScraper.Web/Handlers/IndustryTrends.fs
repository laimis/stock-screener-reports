namespace FinvizScraper.Web.Handlers

module IndustryTrends =
    open FinvizScraper.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open FinvizScraper.Storage
    open FinvizScraper.Web.Shared.Views
    open Giraffe.ViewEngine

    let handler()  =
        let date = FinvizScraper.Core.FinvizConfig.getRunDate()

        let getIndustryUpdatesAndTurnToMap (days:int) =
            date 
            |> Storage.getIndustryUpdates days
            |> List.map (fun x -> (x.industry, x))
            |> Map.ofList
        
        let industryUpdates20 = getIndustryUpdatesAndTurnToMap 20
        let industryUpdates200 = getIndustryUpdatesAndTurnToMap 200

        let dataRows =
            industryUpdates20
            |> Map.toList
            |> List.map (fun (key, iu20) ->

                let toSMACells (update:FinvizScraper.Core.IndustryUpdate) =
                    let total = update.above + update.below

                    let percentAbove =
                        match total with
                        | 0 -> 0.0
                        | _ -> (float update.above ) * 100.0 / (float total)

                    [
                        td [] [ update.above.ToString() |> str  ]
                        td [] [ total.ToString() |> str ]
                        td [] [ System.String.Format("{0:N2}%", percentAbove) |> str ]
                    ]

                let sma20Cells = toSMACells iu20
                let sma200Cells = toSMACells (industryUpdates200[key])

                let image = img [ 
                    _src Links.finvizLogoLink
                    _width "50px"
                ]

                let commonCells = [
                    td [] [ iu20.industry |> Links.industryFinvizLink |> generateHrefWithElement image]
                    td [] [ iu20.industry |> Links.industryLink |> generateHref iu20.industry]
                ]

                let cells = List.append (List.append commonCells sma20Cells) sma200Cells

                tr [] cells
            )

        let header = tr [] [
            th [] [ ]
            th [] [ str "Industry" ]
            th [ _colspan "3"] [ str "20" ]
            th [ _colspan "3"] [ str "200" ]
        ]

        let table = header::dataRows |> fullWidthTable

        let view = [
            div [_class "content"] [
                h1 [] [
                    str "Industry Trends"
                ]
            ]
            table
        ]
        
        view |> mainLayout $"Industry Trends" 