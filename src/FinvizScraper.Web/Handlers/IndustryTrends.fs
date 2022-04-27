namespace FinvizScraper.Web.Handlers

module IndustryTrends =
    open FinvizScraper.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open FinvizScraper.Storage
    open Giraffe.ViewEngine.Attributes
    open FinvizScraper.Web.Shared.Views

    let handler()  =
        let date = FinvizScraper.Core.FinvizConfig.getRunDate()
        let industryUpdates = Storage.getIndustryUpdates date

        let table = 
            industryUpdates
            |> List.map (fun iu ->

                let total = iu.above + iu.below

                let percentAbove =
                    match total with
                    | 0 -> 0.0
                    | _ -> (float iu.above ) * 100.0 / (float total)
                tr [] [
                    td [] [ iu.industry |> Links.industryLink |> generateHref iu.industry]
                    td [] [ iu.above.ToString() |> str  ]
                    td [] [ total.ToString() |> str ]
                    td [] [ System.String.Format("{0:N2}%", percentAbove) |> str ]
                    td [] [ iu.industry |> Links.industryFinvizLink |> generateHref "TradingView"]
                ]
            )
            |> fullWidthTable

        let view = [
            div [_class "content"] [
                h1 [] [
                    str "Industry Trends"
                ]
            ]
            table
        ]
        
        view |> mainLayout $"Industry Trends" 