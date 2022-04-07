namespace FinvizScraper.Web.Handlers

module ScreenersTrends =

    open Giraffe.ViewEngine
    open FinvizScraper.Web.Shared
    open FinvizScraper.Storage
    
    let private days = 14

    let handler() =

        let screeners = Storage.getScreeners()

        let charts = 
            screeners
            |> List.map (fun screener ->
                let data = Reports.getDailyCountsForScreener screener.id days

                data
                |> Charts.convertNameCountsToChart screener.name (Some("80"))
                |> div [_class "block"] 
            )

        let view = 
            div [_class "content"] [
                h1 [] [
                    str "All Screener Trends"
                ]
            ]::charts

        view |> Views.mainLayout "All Screener Trends"