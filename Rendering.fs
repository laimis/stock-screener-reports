namespace FinvizScraper

module Rendering =

    let screenerResultToTr (a:ScreenerResult) =
        
        let toTd input =
            Giraffe.ViewEngine.HtmlElements.td [] [
                Giraffe.ViewEngine.HtmlElements.str input
            ]
        
        let toTdWithHref href =
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
            toTd a.ticker
            toTd a.company
            toTd a.sector
            toTd a.industry
            toTd a.country
            toTd a.marketCap
            toTd a.price
            toTd a.change
            toTd a.volume
            toTdWithHref $"https://tradingview.com/chart/kQn4rgoA/?symbol={a.ticker}"
        ]

    let getHeaderRow =
        let headers = [ 
            "Ticker"; "Company"; "Sector"; "Industry"; "Country"; 
            "Market Cap"; "Price"; "Change"; "Volume"; "Link"
        ]

        let toHeader title =
            Giraffe.ViewEngine.HtmlElements.th [] [
                Giraffe.ViewEngine.HtmlElements.str title
            ]

        let headerCells = headers |> List.map toHeader

        Giraffe.ViewEngine.HtmlElements.tr [] headerCells

    let renderScreenerResultsAsHtml title (screenerResults:seq<ScreenerResult>) =
        
        let headerRow = getHeaderRow
            
        let tickerRows = screenerResults |> Seq.map screenerResultToTr |> Seq.toList

        let tableHtml = Giraffe.ViewEngine.HtmlElements.table [] (headerRow::tickerRows)

        let copyForTradeView = Giraffe.ViewEngine.HtmlElements.div [] [
            Giraffe.ViewEngine.HtmlElements.str (
                screenerResults |> Seq.map (fun x -> x.ticker) |> String.concat ","
            )
        ]

        let view =
            Giraffe.ViewEngine.HtmlElements.html [] [
                Giraffe.ViewEngine.HtmlElements.head [] [ 
                    Giraffe.ViewEngine.HtmlElements.title [] [
                        Giraffe.ViewEngine.HtmlElements.str ("Screener Result for " + title)
                    ]
                ]
                Giraffe.ViewEngine.HtmlElements.body [] [
                    tableHtml
                    copyForTradeView
                ]
            ]

        Giraffe.ViewEngine.RenderView.AsString.htmlDocument view