namespace StockScreenerReports.Web.Handlers

open Giraffe
open StockScreenerReports.Storage
open StockScreenerReports.Core

module Search =
    open StockScreenerReports.Web.Shared.Views
    open Giraffe.ViewEngine
    open StockScreenerReports.Web.Shared
    
    let messageView message =
        let view =
            div [] [ message |> str ]

        [view] |> mainLayout "Search"

    type Name<'a> = { name : string; value : 'a }
    
    type Match =
        | StockMatch of Name<Stock>
        | IndustryMatch of string

    let render query results =
         
        let matchToRow m =
            let cells =
                match m with
                    | StockMatch stock -> 
                        let matchedStock = stock.value

                        [
                            td [] ["Stock" |> str]
                            td [] [Links.stockLink (stock.value.ticker |> StockTicker.value) |> generateHref stock.name]
                            td [] [matchedStock.company |> str]
                            td [] [matchedStock.industry |> Links.industryLink |> generateHref matchedStock.industry]
                            td [] [matchedStock.country |> Links.countryLink |> generateHref matchedStock.country]
                        ]
                    | IndustryMatch industry -> [
                            td [] ["Industry" |> str]
                            td [] [Links.industryLink industry |> generateHref industry]
                            td [_colspan "3"] ["" |> str]
                        ]

            tr [] cells

        let rows = results |> List.map matchToRow

        let headers = ["Type"; "Name"; "";"";"";]

        let table = fullWidthTable headers rows

        div [_class "content"] [
            h3 [] [ str $"Search Results for {query}: {results.Length} match(es)" ]
            table
        ]

    let handler : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->

            match ctx.TryGetQueryStringValue "query" with
            | None -> 
                (messageView "No query parameter provided") next ctx
            
            | Some queryParameter -> 
                    
                let stocks = queryParameter |> Storage.findStocksByTicker |> List.map (fun x -> StockMatch({ name = (x.ticker |> StockTicker.value); value = x }))
                let industries = Storage.getIndustries() |> List.filter (fun x -> x.Contains(queryParameter, System.StringComparison.InvariantCultureIgnoreCase)) |> List.map (fun x -> IndustryMatch(x))

                let results = stocks @ industries

                let view = results |> render queryParameter

                ([view] |> mainLayout "Search Results") next ctx