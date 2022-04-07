namespace FinvizScraper.Web.Handlers

module ScreenersTrends =

    open Giraffe.ViewEngine
    open FinvizScraper.Web.Shared
    open FinvizScraper.Storage
    open System
    
    let private days = 14

    let generateChartElements title labels data =
        let chartGuid = Guid.NewGuid().ToString("N")
        let canvasId = $"chart{chartGuid}"

        let chartDiv = div [] [
            canvas [
                _id canvasId
                attr "aria-label" $"Chart for {title}"
                attr "role" "img"
            ] [
                p [] [ str $"Chart for {title}" ]
            ]
        ]

        let chartScript = script [_type "application/javascript"] [
            Views.generateJSForChart title canvasId labels data
        ]

        [
            chartDiv
            chartScript
        ]

    let convertNameCountsToChart title listOfNameCountPairs =
        let labels = listOfNameCountPairs |> List.map (fun ((name:DateTime),_) -> name.ToString("MMM/dd"))
        let data = listOfNameCountPairs |> List.map (fun (_,count) -> count)

        generateChartElements title labels data

    let handler() =

        let screeners = Storage.getScreeners()

        let charts = 
            screeners
            |> List.map (fun screener ->
                let data = Reports.getDailyCountsForScreener screener.id days

                convertNameCountsToChart screener.name data
            )
            |> List.collect (fun x -> x)

        let view = 
            div [_class "content"] [
                h1 [] [
                    str "All Screener Trends"
                ]
            ]::charts

        view |> Views.mainLayout "All Screener Trends"