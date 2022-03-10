namespace FinvizScraper

module Rendering =
    open Giraffe.ViewEngine.HtmlElements
    open Giraffe.ViewEngine.Attributes

    let tableAttributes = _class "table is-fullwidth"

    let screenerResultToTr (result:ScreenerResult) =
        
        let toTd input =
            td [] [
                str input
            ]
        
        let toTdWithHref href =
            td [] [
                a [
                    _href href
                    _target "_blank"
                ] [
                    str "link"
                ]
            ]

        let rowAttributes = [_height "50px"]

        tr rowAttributes [
            toTd result.ticker
            toTd result.company
            toTd result.sector
            toTd result.industry
            toTd result.country
            toTd result.marketCap
            toTd result.price
            toTd result.change
            toTd result.volume
            toTdWithHref $"https://tradingview.com/chart/kQn4rgoA/?symbol={result.ticker}"
        ]

    let headerRow =
        let headers = [ 
            "Ticker"; "Company"; "Sector"; "Industry"; "Country"; 
            "Market Cap"; "Price"; "Change"; "Volume"; "Link"
        ]

        let toHeader title =
            th [] [str title]

        let headerCells = headers |> List.map toHeader

        tr [] headerCells

    let toBreakdownTable breakdownTitle (breakdown:seq<string * list<ScreenerResult>>) =
        // row with headers for each column and another row with length
        let headerRow = tr [] [
            th [] [str breakdownTitle]
            th [] [str ""]
        ] 

        let valueRows =
            breakdown
            |> Seq.map (fun a ->
                let name = fst(a) 
                let length = snd(a).Length.ToString()
                tr [] [
                    td [] [str name]
                    td [] [str length]
                ]
            )
            |> Seq.toList
        
        table [ tableAttributes ] (headerRow::valueRows)

    let renderScreenerResultsAsHtml 
        pageTitle
        sectorBreakdown
        industryBreakdown
        countryBreakdown
        (screenerResults:seq<ScreenerResult>) =

        let sectorTable = toBreakdownTable "Sectors" sectorBreakdown
        let industryTable = toBreakdownTable "Industries" industryBreakdown
        let countryTable = toBreakdownTable "Countries" countryBreakdown
        
        let tickerRows = screenerResults |> Seq.map screenerResultToTr |> Seq.toList

        let resultTable = table [tableAttributes] (headerRow::tickerRows)

        let copyForTradeView = div [] [
            str (
                screenerResults |> Seq.map (fun x -> x.ticker) |> String.concat ","
            )
        ]

        let view =
            html [] [
                head [] [ 
                    title [] [
                        str ("Screener Result for " + pageTitle)
                    ]
                    link [
                        _rel "stylesheet"
                        _href "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css"
                    ]
                ]
                body [] [
                    div [_class "content"] [
                        h1 [] [
                            str ("Screener: " + pageTitle)
                        ]
                        p [] [
                            str ("Total Results: " + (Seq.length screenerResults).ToString())
                        ]
                    ]
                    div [_class "columns"] [
                        div [_class "column"] [sectorTable]
                        div [_class "column"] [industryTable]
                        div [_class "column"] [countryTable]
                    ]
                    div [_class "block"] [
                        resultTable
                    ]
                    div [_class "block"] [
                        copyForTradeView
                    ]
                ]
            ]

        Giraffe.ViewEngine.RenderView.AsString.htmlDocument view