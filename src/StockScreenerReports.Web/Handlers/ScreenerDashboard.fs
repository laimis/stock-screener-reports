namespace StockScreenerReports.Web.Handlers

open StockScreenerReports.Core
open Giraffe
open Giraffe.ViewEngine
open StockScreenerReports.Storage
open StockScreenerReports.Web.Shared

module ScreenerDashboard =
    
    [<CLIMutable>]
    type UpdateScreenerInput =
        {
            name: string
            url: string
        }

    let private generateBreakdowsElementsForDays screenerId dateRange days =
        let fetchBreakdownData dataSource =
            dataSource screenerId dateRange

        let sectorsData = fetchBreakdownData Reports.topSectorsOverDays
        let industriesData = fetchBreakdownData Reports.topIndustriesOverDays
        let countriesData = fetchBreakdownData Reports.topCountriesOverDays

        let sectorsTable =
            sectorsData
            |> Views.toNameCountTableWithLinks "Sectors" 10 (fun name -> Links.sectorLink name) Views.SectorClicked

        let industriesTable = 
            industriesData
            |> Views.toNameCountTableWithLinks "Industries" 10 (fun name -> Links.industryLink name) Views.IndustryClicked

        let countriesTable =
            countriesData
            |> Views.toNameCountTableWithLinks "Countries" 10 (fun name -> Links.countryLink name) Views.CountryClicked

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

    let generateLayoutForScreener (screener:Screener) =

        let dateRange = ReportsConfig.dateRangeAsStrings()

        let header = 
            form [
                _method "POST"
                _action (screener.id |> Links.screenerLink)
            ] [
                div [ _class "field" ] [
                    label [ _for "name" ] [ str "Name" ]
                    input [
                        _type "text"
                        _name "name"
                        _class "input"
                        _value screener.name
                    ]
                ]
                div [ _class "field" ] [
                    label [ _for "url" ] [ str "Url" ]
                    input [
                        _type "text"
                        _name "url"
                        _class "input"
                        _value screener.url
                    ]
                ]
                // submit button
                input [
                    _type "submit"
                    _class "button is-primary"
                    _value "Update"
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
            
    let createHandler screenerId : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
            task {
                let! input = ctx.BindFormAsync<UpdateScreenerInput>()
                let screener = Storage.getScreenerById screenerId
                match screener with
                | Some _ ->
                    Storage.updateScreener screenerId input.name input.url |> ignore
                    return! redirectTo false (screenerId |> Links.screenerLink) next ctx
                | None ->
                    return! (Views.notFound "Screener not found") next ctx
            }
