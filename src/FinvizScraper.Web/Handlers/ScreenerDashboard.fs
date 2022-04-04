namespace FinvizScraper.Web.Handlers

module ScreenerDashboard =

    open Giraffe.ViewEngine
    open FinvizScraper.Web.Handlers.Shared
    open FinvizScraper.Storage
    
    let private days = 14

    let generateLayoutForScreener (screener:FinvizScraper.Core.Screener) =

        let canvasId = $"myChart{screener.id}"
        let data = Reports.getDailyCountsForScreener screener.id days

        let labels = data |> List.map (fun (d, _) -> d.ToString("MMM/dd"))
        let data = data |> List.map (fun (_,counts) -> counts)

        [
            div [_class "content"] [
                h1 [] [
                    str screener.name
                ]
            ]
            div [] [
                canvas [_id canvasId] []
            ]
            script [_type "application/javascript"] [
                generateJSForChart "Number of results" canvasId labels data
            ]
        ]


    let handler screenerId  = 
        
        let byIdOption = Storage.getScreenerById screenerId
        match byIdOption with
        | Some screener -> 

            let view = generateLayoutForScreener screener
            view |> mainLayout $"Screener: {screener.name}"
        | None ->
            notFound "Screener not found"

    let handlerForAllScreeners() =

        let screeners = Storage.getScreeners()

        let view = generateLayoutForScreener (screeners.Item(0))

        view |> mainLayout "Screener Dashboard"