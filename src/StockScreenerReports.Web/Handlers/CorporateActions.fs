module StockScreenerReports.Web.Handlers.CorporateActions


open System.Collections.Generic
open StockScreenerReports.Core
open Giraffe.ViewEngine.Attributes
open Giraffe.ViewEngine.HtmlElements
open StockScreenerReports.Storage
open StockScreenerReports.Web.Shared
open StockScreenerReports.Web.Shared.Views
open Giraffe

let handler : HttpHandler =
    fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
        task {
            let! corporateActions = Storage.getCorporateActions()

            let stocks =
                corporateActions
                |> List.map (_.Symbol)
                |> List.distinct
                |> Storage.getStockByTickers
                |> List.map (fun stock -> stock.ticker |> StockTicker.value)
                |> HashSet
                
            // get latest job runs 
            let missedJobs =
                Storage.getJobs()
                |> List.filter (fun job ->
                    match job.name with
                    | AlertsJob | CountriesJob | EarningsJob | ScreenerJob | TestJob | TrendsJob -> false
                    | CorporateActionsJob -> true)
                |> List.filter Utils.failedJobFilter
            
            let corporateActionRows =
                corporateActions
                |> List.map (fun action ->
                    let symbolColumn =
                        match stocks.Contains action.Symbol with
                        | true -> TickerLinkColumn(action.Symbol)
                        | false -> StringColumn(action.Symbol)
                        
                    [
                        DateColumn(action.Date |> Utils.convertToDateTime)
                        symbolColumn
                        StringColumn(action.TypeName)
                        StringColumn(action.Action)
                    ] |> toTr
                )

            let headerNames = [
                "Date"
                "Symbol"
                "Type"
                "Action"
            ]

            let corporateActionsTable =
                match corporateActionRows with
                | [] -> div [] [str "No corporate actions found"]
                | _ ->
                    corporateActionRows |> fullWidthTableWithSortableHeaderCells headerNames

            let warningSection = jobAlertSection missedJobs
            
            let view = div [_class "content"] [
                h2 [] [str "Corporate Actions"]
                warningSection
                corporateActionsTable
            ]

            return! ([ view ] |> mainLayout "Corporate Actions Dashboard") next ctx
        }