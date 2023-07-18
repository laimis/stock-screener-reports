namespace StockScreenerReports.Web.Handlers

module ScreenerDashboard =

    open StockScreenerReports.Core
    open Giraffe.ViewEngine
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared
    

    let private generateBreakdowsElementsForDays screenerId dateRange days =
        let fetchBreakdownData dataSource =
            dataSource screenerId dateRange

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

    let generateLayoutForScreener (screener:StockScreenerReports.Core.Screener) =

        let dateRange = ReportsConfig.dateRangeAsStrings()

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
            screener.id
            |> Reports.getDailyCountsForScreener dateRange
            |> Charts.convertNameCountsToChart screener.name Charts.Bar None None (ReportsConfig.getBackgroundColorForScreenerId screener.id)

        let headerWithCharts = header::dailyChart

        let breakdownElements = 
            [7; 14; 30]
            |> List.map (fun days ->
                
                let range = 
                    days
                    |> ReportsConfig.dateRangeWithDays
                    |> ReportsConfig.formatDateRangeToStrings
                
                days |> generateBreakdowsElementsForDays screener.id range
            )
            |> List.concat

        let results =
            screener.id |>
            Reports.getScreenerResultsForDays dateRange

        let resultsTable =
            results
            |> ScreenerResults.generateScreenerResultTable [] []

        headerWithCharts @ breakdownElements @ [resultsTable]


    let handler screenerId  = 
        
        let byIdOption = Storage.getScreenerById screenerId
        match byIdOption with
        | Some screener -> 

            let view = generateLayoutForScreener screener
            view |> Views.mainLayout $"Screener: {screener.name}"
        | None ->
            Views.notFound "Screener not found"
