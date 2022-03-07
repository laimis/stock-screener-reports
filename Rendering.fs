namespace FinvizScraper

module Rendering =

    let toHtml (a:ScreenerResult) =
        
        Giraffe.ViewEngine.HtmlElements.tr [] [
            Giraffe.ViewEngine.HtmlElements.td [] [
                Giraffe.ViewEngine.HtmlElements.str a.ticker
            ]

            Giraffe.ViewEngine.HtmlElements.td [] [
                Giraffe.ViewEngine.HtmlElements.str a.company
            ]

            Giraffe.ViewEngine.HtmlElements.td [] [
                Giraffe.ViewEngine.HtmlElements.str a.sector
            ]

            Giraffe.ViewEngine.HtmlElements.td [] [
                Giraffe.ViewEngine.HtmlElements.str a.industry
            ]

            Giraffe.ViewEngine.HtmlElements.td [] [
                Giraffe.ViewEngine.HtmlElements.str a.country
            ]

            Giraffe.ViewEngine.HtmlElements.td [] [
                Giraffe.ViewEngine.HtmlElements.str a.marketCap
            ]

            Giraffe.ViewEngine.HtmlElements.td [] [
                Giraffe.ViewEngine.HtmlElements.str a.price
            ]

            Giraffe.ViewEngine.HtmlElements.td [] [
                Giraffe.ViewEngine.HtmlElements.str a.change
            ]

            Giraffe.ViewEngine.HtmlElements.td [] [
                Giraffe.ViewEngine.HtmlElements.str a.volume
            ]
        ]

    let generateHtml (screenerResults:seq<ScreenerResult>) =
        
        let tickerRows = screenerResults |> Seq.map toHtml |> Seq.toList

        let view =
            Giraffe.ViewEngine.HtmlElements.html [] [
                Giraffe.ViewEngine.HtmlElements.head [] [ 
                    Giraffe.ViewEngine.HtmlElements.title [] [
                        Giraffe.ViewEngine.HtmlElements.str "Giraffe" ]
                ]
                Giraffe.ViewEngine.HtmlElements.body [] [
                    Giraffe.ViewEngine.HtmlElements.table [] tickerRows
                ]
            ]

        Giraffe.ViewEngine.RenderView.AsString.htmlDocument view