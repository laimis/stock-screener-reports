namespace StockScreenerReports.Web.Handlers

module StockManagement =
    open Giraffe
    open Giraffe.ViewEngine
    open StockScreenerReports.Web.Shared.Views
    open StockScreenerReports.Storage
    open StockScreenerReports.Core

    [<CLIMutable>]
    type StockTickerAdjustment =
        {
            oldSymbol : string
            newSymbol : string
        }

    let handlerWithMessageBlock messageBlock =
        
        let messageOrBlankSpan =
            match messageBlock with
            | Some b -> b
            | None -> span [] []

        let tickerBlock = div [_class "block"] [
            h2 [] [
                "Adjust Ticker Symbol" |> str
            ]
            form [ _action "/stocks/adjustticker"; _method "post" ] [
                div [_class "field"] [
                    label [_class "label"] [ str "Old Symbol" ]
                    div [_class "control"] [
                        input [ _type "text"; _name "oldSymbol"; _class "input" ]
                    ]
                ]
                div [_class "field"] [
                    label [_class "label"] [ str "New Symbol" ]
                    div [_class "control"] [
                        input [ _type "text"; _name "newSymbol"; _class "input" ]
                    ]
                ]
                div [_class "field"] [
                    div [_class "control"] [
                        button [ _type "submit"; _class "button is-primary" ] [ str "Adjust" ]
                    ]
                ]
            ]
        ]

        let view = div [_class "content"] [
            h1 [] [
                "Stock Management" |> str
            ]
            messageOrBlankSpan
            tickerBlock
        ]

        [view] |> mainLayout "Stock Management"

    let handler() =
        // block for adjusting ticker symbol, we need a form with two inputs: old symbol and new symbol
        
        handlerWithMessageBlock None

    let adjustTicker : HttpHandler =

        let messageSection messageTypeClass message = 
            div [_class $"message {messageTypeClass}"] [
                div [_class "message-body"] [
                    message |> str
                ]
            ]

        let successMessageSection = messageSection "is-success"
        let errorMessageSection = messageSection "is-danger"

        fun (next: HttpFunc) (ctx: Microsoft.AspNetCore.Http.HttpContext) ->
            task {
                let! adjustmentInfo = ctx.TryBindFormAsync<StockTickerAdjustment>()
                
                let status = 
                    match adjustmentInfo with
                    | Ok adjustmentInfo ->

                        let oldStock = adjustmentInfo.oldSymbol |> StockTicker.create |> Storage.getStockByTicker
                        match oldStock with
                        | None -> 
                            $"could not find stock with ticker {adjustmentInfo.oldSymbol}" |> errorMessageSection
                            
                        | Some _ ->

                            let newStovk = adjustmentInfo.newSymbol |> StockTicker.create |> Storage.getStockByTicker
                            match newStovk with
                            | Some _ -> 
                                $"stock with ticker {adjustmentInfo.newSymbol} already exists" |> errorMessageSection
                                
                            | None ->

                                let oldTicker = adjustmentInfo.oldSymbol |> StockTicker.create
                                let newTicker = adjustmentInfo.newSymbol |> StockTicker.create
                                let updates = Storage.updateStockTicker oldTicker newTicker
                                $"Updated {updates} records" |> successMessageSection

                    | Error err ->
                        $"Stock Management: failed to parse:  {err}" |> errorMessageSection
                
                return! (handlerWithMessageBlock (Some status)) next ctx
            }