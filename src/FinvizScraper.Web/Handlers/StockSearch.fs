namespace FinvizScraper.Web.Handlers

open Giraffe

module StockSearch =

    let redirect : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
            let someValue =
                match ctx.TryGetQueryStringValue "ticker" with
                | None -> "notfound"
                | Some headerValue -> headerValue.ToUpper()

            (redirectTo false $"/stocks/{someValue}") next ctx