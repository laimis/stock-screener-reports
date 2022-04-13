namespace FinvizScraper.Web.Handlers

module ScreenersTrends =

    open Giraffe.ViewEngine
    open FinvizScraper.Web.Shared
    open FinvizScraper.Storage
    
    let handler() =

        let days = FinvizScraper.Core.FinvizConfig.dayRange
        
        let screeners = Storage.getScreeners()

        let charts = 
            screeners
            |> List.map (fun screener ->
                let data = Reports.getDailyCountsForScreener screener.id days

                data
                |> Charts.convertNameCountsToChart screener.name Charts.smallChart
                |> div [_class "block"] 
            )

        let view = 
            div [_class "content"] [
                h1 [] [
                    str "All Screener Trends"
                ]
            ]::charts

        view |> Views.mainLayout "All Screener Trends"