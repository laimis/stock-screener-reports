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

        let chartDiv = div [] [
            canvas [_id canvasId] []
        ]

        let sectorsData = Reports.topSectorsOverDays screener.id (System.DateTime.Now.AddDays(-days)) System.DateTime.Now
        let industriesData = Reports.topIndustriesOverDays screener.id (System.DateTime.Now.AddDays(-days)) System.DateTime.Now
        let countriesData = Reports.topCountriesOverDays screener.id (System.DateTime.Now.AddDays(-days)) System.DateTime.Now

        let sectorsTable = sectorsData |> toNameCountTable "Sectors"
        let industriesTable = industriesData |> toNameCountTable "Industries"
        let countriesTable = countriesData |> toNameCountTable "Countries"

        let breadownDiv = div [_class "columns"] [
            div [_class "column"] [sectorsTable]
            div [_class "column"] [industriesTable]
            div [_class "column"] [countriesTable]
        ]

        [
            div [_class "content"] [
                h1 [] [
                    str screener.name
                ]
            ]
            chartDiv
            breadownDiv
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