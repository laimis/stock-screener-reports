namespace FinvizScraper

module Rendering =

    let tableAttributes = Giraffe.ViewEngine.Attributes._class "table is-fullwidth"

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

    let headerRow =
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

    let toBreakdownTable (breakdown:seq<string * list<ScreenerResult>>) =
        // row with headers for each column and another row with length
        let headerCells = 
            breakdown
            |> Seq.map (fun a -> fst(a))
            |> Seq.map (fun a -> Giraffe.ViewEngine.HtmlElements.th [] [Giraffe.ViewEngine.HtmlElements.str a])
            |> Seq.toList

        let valueCells =
            breakdown
            |> Seq.map (fun a -> snd(a))
            |> Seq.map (fun a -> Giraffe.ViewEngine.HtmlElements.td [] [Giraffe.ViewEngine.HtmlElements.str (string a.Length)])
            |> Seq.toList
        
        Giraffe.ViewEngine.HtmlElements.table [ tableAttributes ] [
            Giraffe.ViewEngine.HtmlElements.tr [] headerCells
            Giraffe.ViewEngine.HtmlElements.tr [] valueCells
        ]

    let renderScreenerResultsAsHtml 
        title
        sectorBreakdown
        industryBreakdown
        countryBreakdown
        (screenerResults:seq<ScreenerResult>) =

        let sectorTable = toBreakdownTable sectorBreakdown
        let industryTable = toBreakdownTable industryBreakdown
        let countryTable = toBreakdownTable countryBreakdown
        
        let tickerRows = screenerResults |> Seq.map screenerResultToTr |> Seq.toList

        let resultTable = Giraffe.ViewEngine.HtmlElements.table [tableAttributes] (headerRow::tickerRows)

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
                    Giraffe.ViewEngine.HtmlElements.link [
                        Giraffe.ViewEngine.Attributes._rel "stylesheet"
                        Giraffe.ViewEngine.Attributes._href "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css"
                    ]
                ]
                Giraffe.ViewEngine.HtmlElements.body [] [
                    sectorTable
                    Giraffe.ViewEngine.HtmlElements.br []
                    industryTable
                    Giraffe.ViewEngine.HtmlElements.br []
                    countryTable
                    Giraffe.ViewEngine.HtmlElements.br []
                    resultTable
                    Giraffe.ViewEngine.HtmlElements.br []
                    copyForTradeView
                ]
            ]

        Giraffe.ViewEngine.RenderView.AsString.htmlDocument view