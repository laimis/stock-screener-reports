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

    let createHtmlPage
        pageTitle
        linkAttributes
        contents =

        let view =
            html [] [
                head [] [ 
                    title [] [str pageTitle]
                    link linkAttributes
                ]
                body [] contents
            ]

        Giraffe.ViewEngine.RenderView.AsString.htmlDocument view


    let renderScreenerResultsAsHtml 
        (screenerInput:ScreenerInput)
        breakdowns
        (screenerResults:seq<ScreenerResult>) =

        let breakdownDivs = 
            breakdowns
            |> Seq.map (fun a -> toBreakdownTable (fst(a)) (snd(a)))
            |> Seq.map (fun a -> div [_class "column"] [a])
            |> Seq.toList

        let tickerRows = screenerResults |> Seq.map screenerResultToTr |> Seq.toList

        let resultTable = table [tableAttributes] (headerRow::tickerRows)

        let copyForTradeView = div [] [
            str (
                screenerResults |> Seq.map (fun x -> x.ticker) |> String.concat ","
            )
        ]

        let linkSection = [
            _rel "stylesheet"
            _href "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css"
        ]

        let content = [
            div [_class "content"] [
                h1 [] [
                    str ("Screener: " + screenerInput.name)
                ]
                div [_class "block"] [
                    str ("Total Results: " + (Seq.length screenerResults).ToString())
                ]
                div [_class "block"] [
                    a [ 
                        _href screenerInput.url
                        _target "_blank"
                    ] [
                        str "View on Finviz"
                    ]
                ]
            ]
            div [_class "columns"] breakdownDivs
            div [_class "block"] [resultTable]
            div [_class "block"] [copyForTradeView]
        ]

        createHtmlPage ("Screener Result for " + screenerInput.name) linkSection content

    let renderIndex inputs =
        // just body with three divs each with an href to the file

        let links = 
            inputs 
            |> Seq.map (fun x ->
                div [] [
                    a [
                        _href x.filename
                        _target "_blank"
                    ] [
                        str x.name
                    ]
                ]
            )|> Seq.toList

        createHtmlPage "Screener Index" [] links