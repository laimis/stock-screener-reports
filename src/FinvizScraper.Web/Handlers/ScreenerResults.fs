namespace FinvizScraper.Web.Handlers

module ScreenerResults =

    open Giraffe.ViewEngine
    open FinvizScraper.Storage.Reports
    open FinvizScraper.Web.Shared

    let screenerResultToTr (result:ScreenerResultReportItem) =
        
        let toTd input =
            td [] [ str input ]

        let toTdWithNode node =
            td [] [ node ]

        let rowAttributes = [_height "50px"]

        tr rowAttributes [
            toTdWithNode (Views.generateTickerLink result.ticker)
            toTd result.name
            toTd result.sector
            toTd result.industry
            toTd result.country
            toTd (string result.marketCap)
            toTd (string result.price)
            toTd (string result.change)
            toTd (string (result.volume.ToString("N0")))
            result.ticker |> Links.tradingViewLink |> Views.generateHref "link" |> toTdWithNode
        ]

    let toBreakdownTable breakdownTitle (breakdown:seq<string * list<ScreenerResultReportItem>>) =
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
        
        headerRow::valueRows |> Views.fullWidthTable
        
    let calculateBreakdowns screenerResults =
        
        let convertToBreakdown (name, groupByProperty) =
            (name, (screenerResults |> List.groupBy groupByProperty |> List.sortByDescending (fun (_, list) -> list.Length)))

        let breakdowns = [
            ("Sectors", fun a -> a.sector);
            ("Industries", fun a -> a.industry);
            ("Countries", fun a -> a.country);
        ]
        
        breakdowns |> List.map convertToBreakdown

    let headerRow =
        let headers = [ 
            "Ticker"
            "Company"
            "Sector"
            "Industry"
            "Country" 
            "Market Cap"
            "Price"
            "Change"
            "Volume"
            "Link"
        ]

        let toHeader title =
            th [] [str title]

        let headerCells = headers |> List.map toHeader

        tr [] headerCells

    let private view (screener:FinvizScraper.Core.Screener) (results:list<ScreenerResultReportItem>) =

        let screenerTable =
            results
            |> List.map screenerResultToTr
            |> List.append [headerRow]
            |> Views.fullWidthTable

        let breakdowns = calculateBreakdowns results
                
        let breakdownDivs = 
            breakdowns
            |> List.map (fun (breakdownTitle,breakdownList) -> toBreakdownTable breakdownTitle breakdownList)
            |> List.map (fun breakdownTable -> div [_class "column"] [breakdownTable])

        [
            div [_class "content"] [
                h1 [] [
                    str ("Screener: " + screener.name)
                ]
                div [_class "block"] [
                    str ("Total Results: " + (string results.Length))
                ]
                
                div [_class "block"] [
                    a [
                        _class "button is-primary" 
                        _href screener.url
                        _target "_blank"
                    ] [
                        str "View on Finviz"
                    ]
                ]
            ]
            div [_class "columns"] breakdownDivs
            div [_class "block"] [screenerTable]
        ]

    let handler ((id:int),(date:string))  = 
        
        // get screeners, render them in HTML
        let byIdOption = FinvizScraper.Storage.Storage.getScreenerById id
        match byIdOption with
        | Some screener -> 
            let screenerResults = getScreenerResults screener.id date
            let view      = view screener screenerResults
            view |> Views.mainLayout $"Screener: {screener.name}"
        | None ->
            Views.notFound "Screener not found"