namespace FinvizScraper.Web.Handlers

module ScreenerDashboard =

    open Giraffe.ViewEngine
    open FinvizScraper.Web.Shared
    open FinvizScraper.Storage
    open System
    
    let private days = 14

    let generateLayoutForScreener (screener:FinvizScraper.Core.Screener) =

        let data = Reports.getDailyCountsForScreener screener.id days

        let chartElements = Views.convertNameCountsToChart screener.name data

        let fetchBreakdownData dataSource =
            let startDate = DateTime.Now.AddDays(-days)
            let endDate = DateTime.Now

            dataSource screener.id startDate endDate

        let sectorsData = fetchBreakdownData Reports.topSectorsOverDays
        let industriesData = fetchBreakdownData Reports.topIndustriesOverDays
        let countriesData = fetchBreakdownData Reports.topCountriesOverDays

        let sectorsTable = sectorsData |> Views.toNameCountTable "Sectors"
        let industriesTable = industriesData |> Views.toNameCountTable "Industries"
        let countriesTable = countriesData |> Views.toNameCountTable "Countries"

        let breakdownDiv = div [_class "columns"] [
            div [_class "column"] [sectorsTable]
            div [_class "column"] [industriesTable]
            div [_class "column"] [countriesTable]
        ]

        let header = 
            div [_class "content"] [
                h1 [] [
                    str screener.name
                ]
            ]

        let headerWithCharts = header::chartElements

        [breakdownDiv] |> List.append headerWithCharts


    let handler screenerId  = 
        
        let byIdOption = Storage.getScreenerById screenerId
        match byIdOption with
        | Some screener -> 

            let view = generateLayoutForScreener screener
            view |> Views.mainLayout $"Screener: {screener.name}"
        | None ->
            Views.notFound "Screener not found"