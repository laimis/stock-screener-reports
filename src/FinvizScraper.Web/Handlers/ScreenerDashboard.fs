namespace FinvizScraper.Web.Handlers

module ScreenerDashboard =

    open Giraffe.ViewEngine
    open FinvizScraper.Web.Shared
    open FinvizScraper.Storage
    open System
    

    let private generateBreakdowsElementsForDays screenerId (days:int) =
        let fetchBreakdownData dataSource =
            let startDate = DateTime.Now.AddDays(-days)
            let endDate = DateTime.Now

            dataSource screenerId startDate endDate

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

        let breakdownHeader = div [_class "content mt-5"] [
            h2 [] [
                str $"Last {days} days"
            ]
        ]

        [
            breakdownHeader;
            breakdownDiv
        ]

    let generateLayoutForScreener (screener:FinvizScraper.Core.Screener) =

        let days = FinvizScraper.Core.FinvizConfig.dayRange

        

        let header = 
            div [_class "content"] [
                h1 [] [
                    str screener.name
                ]
            ]

        let last30DaysChartElements =
            days
            |> Reports.getDailyCountsForScreener screener.id
            |> Charts.convertNameCountsToChart screener.name None

        let headerWithCharts = header::last30DaysChartElements

        // final page has
        // title, chart of 30 days, last 7, last 14, last 30

        let breakdownElements = 
            [7; 14; 30]
            |> List.map (fun days -> days |> generateBreakdowsElementsForDays screener.id)
            |> List.concat

        breakdownElements |> List.append headerWithCharts


    let handler screenerId  = 
        
        let byIdOption = Storage.getScreenerById screenerId
        match byIdOption with
        | Some screener -> 

            let view = generateLayoutForScreener screener
            view |> Views.mainLayout $"Screener: {screener.name}"
        | None ->
            Views.notFound "Screener not found"