namespace FinvizScraper

module Rendering =

    let toHtml (a:ScreenerResult) =
        
        let toCell input =
            Giraffe.ViewEngine.HtmlElements.td [] [
                Giraffe.ViewEngine.HtmlElements.str input
            ]
        
        let toCellWithHref href =
            Giraffe.ViewEngine.HtmlElements.td [] [
                Giraffe.ViewEngine.HtmlElements.a [
                    Giraffe.ViewEngine.Attributes._href href
                    Giraffe.ViewEngine.Attributes._target "_blank"
                ] [
                    Giraffe.ViewEngine.HtmlElements.str "link"
                ]
            ]

        let rowAttributes = [Giraffe.ViewEngine.Attributes._height "50px"]

        Giraffe.ViewEngine.HtmlElements.tr rowAttributes [
            toCell a.ticker
            toCell a.company
            toCell a.sector
            toCell a.industry
            toCell a.country
            toCell a.marketCap
            toCell a.price
            toCell a.change
            toCell a.volume
            toCellWithHref $"https://tradingview.com/chart/kQn4rgoA/?symbol={a.ticker}"
        ]

    let generateHtml title (screenerResults:seq<ScreenerResult>) =
        
        let tickerRows = screenerResults |> Seq.map toHtml |> Seq.toList

        let tableHtml = Giraffe.ViewEngine.HtmlElements.table [] tickerRows

        let copyForTradeView = Giraffe.ViewEngine.HtmlElements.div [] [
            Giraffe.ViewEngine.HtmlElements.str (
                screenerResults |> Seq.map (fun x -> x.ticker) |> Microsoft.FSharp.Core.String.concat ","
            )
        ]

        let view =
            Giraffe.ViewEngine.HtmlElements.html [] [
                Giraffe.ViewEngine.HtmlElements.head [] [ 
                    Giraffe.ViewEngine.HtmlElements.title [] [
                        Giraffe.ViewEngine.HtmlElements.str ("Screener Result for " + title) ]
                ]
                Giraffe.ViewEngine.HtmlElements.body [] [
                    tableHtml
                    copyForTradeView
                ]
            ]

        Giraffe.ViewEngine.RenderView.AsString.htmlDocument view