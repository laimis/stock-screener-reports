namespace StockScreenerReports.Web.Handlers

open Giraffe
open StockScreenerReports.Storage
open StockScreenerReports.Core

module StockSearch =
    open StockScreenerReports.Web.Shared
    
    
    let redirect : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
            let redirectUrl =
                match ctx.TryGetQueryStringValue "ticker" with
                | None -> "notfound" |> Links.stockLink
                | Some headerValue -> 
                    let toSearch = headerValue.ToUpper()
                    let stock = toSearch |> StockTicker.create |> Storage.getStockByTicker

                    match stock with
                    | None -> 
                        let industry = Storage.getIndustries() |> List.tryFind (fun x -> x.Equals(toSearch, System.StringComparison.InvariantCultureIgnoreCase))
                        match industry with
                        | None -> "notfound" |> Links.industryLink
                        | Some industry -> industry |> Links.industryLink

                    | Some stock ->
                        stock.ticker |> StockTicker.value |> Links.stockLink
                    


            (redirectTo false redirectUrl) next ctx