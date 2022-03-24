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
            toTd (string result.price)
            toTd (string result.change)
            toTd (string (result.volume.ToString("N0")))
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
                let (name, list) = a
                tr [] [
                    td [] [str name]
                    td [] [str (list.Length.ToString())]
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


    let renderResultsAsHtml 
        (screenerInput:ScreenerInput)
        breakdownConfig
        (screenerResults:list<ScreenerResult>) =

        let breakdowns =
            breakdownConfig
            |> List.map (
                fun x -> 
                    (x.name, (screenerResults |> Processing.resultBreakdown x.breakdown))
                )
                
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

        let html = createHtmlPage ("Screener Result for " + screenerInput.name) linkSection content
        (screenerInput, screenerResults, html)

    let createIndexPage 
        (configAndResults:seq<ScreenerInput*list<ScreenerResult>>) =
        // just body with three divs each with an href to the file

        let rows = 
            configAndResults
            |> Seq.map (fun x ->
                let (config, results) = x
                tr [] [
                    td [] [
                        a [
                            _href (System.IO.Path.GetFileName(config.filename))
                            _target "_blank"
                        ] [
                            str config.name
                        ]
                    ]
                    td [] [
                        str (results.Length.ToString())
                    ]
                ]
            )|> Seq.toList

        let content = table [] rows

        createHtmlPage "Screener Index" [] [
            div [] [
                str ("Generated by Finviz Scraper on " + System.DateTime.Now.ToString())
            ]
            content
        ]