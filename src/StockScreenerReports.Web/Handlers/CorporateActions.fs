module StockScreenerReports.Web.Handlers.CorporateActions


open System.Collections.Generic
open Microsoft.Extensions.Logging
open StockScreenerReports.Core
open Giraffe.ViewEngine.Attributes
open Giraffe.ViewEngine.HtmlElements
open StockScreenerReports.Storage
open StockScreenerReports.Web.Services
open StockScreenerReports.Web.Shared
open StockScreenerReports.Web.Shared.Utils
open StockScreenerReports.Web.Shared.Views
open Giraffe

let private actionToTickers a =
    match a.Type with
    | CorporateActionType.SymbolChange (oldS, newS) -> [oldS; newS]
    | _ -> [a.Symbol]

let handler : HttpHandler =
    fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
        task {
            let! corporateActions = Storage.getCorporateActions()

            let stocks =
                corporateActions
                |> List.map actionToTickers
                |> List.concat
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
                |> List.filter failedJobFilter
            
            let corporateActionRows =
                corporateActions
                |> List.map (fun action ->
                    
                    let tickerLinks =
                        action |> actionToTickers |> List.map (fun ticker ->
                            match stocks.Contains ticker with
                            | false -> span [_class "mr-2"] [str ticker]
                            | true ->
                                let link = Links.stockLink ticker
                                a [_href link; _class "mr-2"] [str ticker]
                        )
                        
                    [
                        DateColumn(action.Date)
                        NodeColumn(div [] tickerLinks)
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
        
let delistingProcessing : HttpHandler =
    fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
        task {
        
            let processor = CorporateActionProcessor(ctx.GetService<ILogger<CorporateActionProcessor>>())
            let! stocks,recordsDeleted = processor.BankruptyDelistings()
            
            let view = div [_class "content"] [
                h2 [] [str "Delisting Processing"]
                p [] [str "Delisting processing has been initiated"]
                p [] [str "Please wait for the process to complete"]
                p [] [str ("Number of records deleted: " + recordsDeleted.ToString())]
                p [] [str ("Number of stocks processed: " + stocks.Length.ToString())]
                p [] (stocks |> List.map (fun stock -> div [] [stock.company |> str]))                
            ]
            
            return! ([ view ] |> mainLayout "Delisting Processing") next ctx
        }