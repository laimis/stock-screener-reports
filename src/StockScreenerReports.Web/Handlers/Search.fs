namespace StockScreenerReports.Web.Handlers

open Giraffe
open StockScreenerReports.Storage
open StockScreenerReports.Core

module StockSearch =
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Views
    open Giraffe.ViewEngine
    
    let messageView message =
        let view =
            div [] [ message |> str ]

        [view] |> mainLayout "Search"

    let redirect : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->

            match ctx.TryGetQueryStringValue "ticker" with
            | None -> (messageView "Search parameter not provided") next ctx
            | Some headerValue -> 
                    
                let stock =
                    headerValue
                    |> StockTicker.create
                    |> Storage.getStockByTicker

                let redirectOpt =
                    match stock with
                    | None -> 
                        let industry =
                            Storage.getIndustries()
                            |> List.tryFind (fun x -> x.Contains(headerValue, System.StringComparison.InvariantCultureIgnoreCase))

                        match industry with
                        | None -> None
                        | Some industry -> industry |> Links.industryLink |> Some 

                    | Some stock ->
                        stock.ticker |> StockTicker.value |> Links.stockLink |> Some

                match redirectOpt with
                | None -> (messageView $"Unable to find stock or industry matching {headerValue}") next ctx
                | Some redirectUrl ->                            
                    (redirectTo false redirectUrl) next ctx