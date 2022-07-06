namespace FinvizScraper.Web.Handlers

module ScreenerDashboard =

    open Giraffe.ViewEngine
    open FinvizScraper.Web.Shared
    open FinvizScraper.Storage
    open FinvizScraper.Core
    open System
    

    let private generateBreakdowsElementsForDays screenerId (days:int) =
        let fetchBreakdownData dataSource =
            let startDate = DateTime.Now.AddDays(-days)
            let endDate = DateTime.Now

            dataSource screenerId startDate endDate

        let sectorsData = fetchBreakdownData Reports.topSectorsOverDays
        let industriesData = fetchBreakdownData Reports.topIndustriesOverDays
        let countriesData = fetchBreakdownData Reports.topCountriesOverDays

        let sectorsTable =
            sectorsData
            |> Views.toNameCountTableWithLinks "Sectors" 10 (fun name -> Links.sectorLink name)

        let industriesTable = 
            industriesData
            |> Views.toNameCountTableWithLinks "Industries" 10 (fun name -> Links.industryLink name)

        let countriesTable =
            countriesData
            |> Views.toNameCountTableWithLinks "Countries" 10 (fun name -> Links.countryLink name)

        let breakdownDiv = div [_class "columns"] [
            div [_class "column"] [sectorsTable]
            div [_class "column"] [industriesTable]
            div [_class "column"] [countriesTable]
        ]

        let breakdownHeader = div [_class "content mt-5"] [
            h2 [] [str $"Last {days} days"]
        ]

        [
            breakdownHeader;
            breakdownDiv
        ]

    let generateLayoutForScreener (screener:FinvizScraper.Core.Screener) =

        let days = FinvizConfig.dayRange

        let header = 
            div [_class "content"] [
                h1 [] [
                    str $"Screener: {screener.name}"
                ]
                small [] [
                    screener.url |> Views.generateHrefNewTab screener.url
                ]
            ]

        let dailyChart =
            days
            |> Reports.getDailyCountsForScreener screener.id
            |> Charts.convertNameCountsToChart screener.name Charts.Bar None None (FinvizConfig.getBackgroundColorForScreenerId screener.id)

        let headerWithCharts = header::dailyChart

        let breakdownElements = 
            [7; 14; 30]
            |> List.map (fun days -> days |> generateBreakdowsElementsForDays screener.id)
            |> List.concat

        let results =
            days |>
            Reports.getScreenerResultsForDays screener.id

        let resultsTable =
            results
            |> ScreenerResults.generateScreenerResultTable

        headerWithCharts @ breakdownElements @ [resultsTable]


    let handler screenerId  = 
        
        let byIdOption = Storage.getScreenerById screenerId
        match byIdOption with
        | Some screener -> 

            let view = generateLayoutForScreener screener
            view |> Views.mainLayout $"Screener: {screener.name}"
        | None ->
            Views.notFound "Screener not found"