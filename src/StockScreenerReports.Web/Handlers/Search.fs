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

    let private getLink m =
            match m with
                | StockMatch stock -> Links.stockLink (stock.value.ticker |> StockTicker.value)
                | IndustryMatch industry -> Links.industryLink industry

    let redirectFirstResult result =
        let link = result |> getLink
        redirectTo false link

    let renderMultipleResults query results =
        let matchToRow m =
            let cells =
                match m with
                    | StockMatch stock -> 
                        let matchedStock = stock.value

                        [
                            StringColumn("Stock")
                            LinkColumn(stock.name, m |> getLink)
                            StringColumn(matchedStock.company)
                            LinkColumn(matchedStock.industry, matchedStock.industry |> Links.industryLink)
                            LinkColumn(matchedStock.country, matchedStock.country |> Links.countryLink)
                        ]
                    | IndustryMatch industry -> [
                            StringColumn("Industry")
                            LinkColumn(industry, (m |> getLink))
                            StringColumn("")
                            StringColumn("")
                            StringColumn("")
                        ]

            cells |> toTr

        let rows = results |> List.map matchToRow

        let headers = ["Type"; "Name"; "";"";"";]

        let table = fullWidthTable headers rows

        div [_class "content"] [
            h3 [] [ str $"Search Results for {query}: {results.Length} match(es)" ]
            table
        ]

    let render query results =

        match results with
        | [] -> 
            messageView $"No results found for {query}"
        | [x] -> 
            redirectFirstResult x
        | _ ->
            let view = renderMultipleResults query results
            [view] |> mainLayout "Search Results"

    let handler : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->

            match ctx.TryGetQueryStringValue "query" with
            | None -> 
                (messageView "No query parameter provided") next ctx
            
            | Some queryParameter -> 
                    
                let stocks = queryParameter |> Storage.findStocksByTickerOrName |> List.map (fun x -> StockMatch({ name = (x.ticker |> StockTicker.value); value = x }))
                let industries = Storage.getIndustries() |> List.filter (fun x -> x.Contains(queryParameter, System.StringComparison.InvariantCultureIgnoreCase)) |> List.map (fun x -> IndustryMatch(x))

                let results = stocks @ industries

                let nextFunc = results |> render queryParameter

                nextFunc next ctx