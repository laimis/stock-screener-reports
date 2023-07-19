namespace StockScreenerReports.Web.Handlers

module ScreenerResults =

    open Giraffe.ViewEngine
    open StockScreenerReports.Storage.Reports
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Views
    open StockScreenerReports.Core

    let screenerResultToTr tickersWithEarnings topGainers (result:ScreenerResultReportItem) =
        
        let hasEarnings = tickersWithEarnings |> List.contains result.ticker
        let isTopGainer = topGainers |> List.contains result.ticker

        let earningsIcon = hasEarnings |> generateEarningsIcon
        let fireIcon = isTopGainer |> generateTopGainerIcon
            
        let rowAttributes = [_height "50px"]

        tr rowAttributes [
            result.ticker |> generateTickerLink |> toTdWithNode
            [earningsIcon; fireIcon] |> toTdWithNodes
            result.name |> toTd 
            result.sector |> Links.sectorLink |> generateHref result.sector |> toTdWithNode
            result.industry |> Links.industryLink |> generateHref result.industry |> toTdWithNode
            result.country |> Links.countryLink |> generateHref result.country |> toTdWithNode
            result.marketCap |> marketCapFormatted |> toTd
            result.price |> dollarFormatted |> toTd
            result.change |> percentFormatted |> toTd
            result.volume |> volumeFormatted |> toTd
            result.ticker |> Links.tradingViewLink |> generateHrefNewTab "chart" |> toTdWithNode
        ]
    
    let calculateBreakdowns (screenerResults:list<ScreenerResultReportItem>) =
        
        let convertToBreakdown (name, linkFunction, groupByProperty) =
            (name, linkFunction, (screenerResults |> List.groupBy groupByProperty |> List.sortByDescending (fun (_, list) -> list.Length)) |> List.map (fun (key, list) -> (key, list.Length)))

        let breakdowns = [
            ("Sectors", Links.sectorLink, fun (a:ScreenerResultReportItem) -> a.sector);
            ("Industries", Links.industryLink, fun (a:ScreenerResultReportItem) -> a.industry);
            ("Countries", Links.countryLink, fun (a:ScreenerResultReportItem) -> a.country);
        ]
        
        breakdowns |> List.map convertToBreakdown

    let generateScreenerResultTable tickersWithEarnings topGainers results =
        let headers = [
            "Ticker"
            "" 
            "Company"
            "Sector"
            "Industry"
            "Country" 
            "Market Cap"
            "Price"
            "Change"
            "Volume"
            "Chart"
        ]

        results
            |> List.map (fun r -> screenerResultToTr tickersWithEarnings topGainers r)
            |> fullWidthTableWithSortableHeaderCells headers

    let private view
        (screener:StockScreenerReports.Core.Screener)
        (results:list<ScreenerResultReportItem>)
        (allTickersWithEarnings:list<string>)
        (topGainers:list<string>)
        (previousHits:Set<string>) =

        let screenerTable = results |> generateScreenerResultTable allTickersWithEarnings topGainers

        let freshResults = results |> List.filter (fun r -> not (previousHits |> Set.contains r.ticker))

        let freshHitsTable = freshResults |> generateScreenerResultTable allTickersWithEarnings topGainers

        let tickersWithEarningsInResults = results |> List.map (fun r -> r.ticker) |> List.filter (fun t -> allTickersWithEarnings |> List.contains t)

        let breakdowns = calculateBreakdowns results

        let tickers = results |> List.map (fun r -> r.ticker)

        let date = results |> List.head |> (fun r -> r.date) |> Utils.convertToDateString
                
        let breakdownDivs = 
            breakdowns
            |> List.map (
                fun (breakdownTitle,linkFunction,breakdownList) ->
                    toNameCountTableWithLinks breakdownTitle 10 linkFunction breakdownList
                )
            |> List.map (
                fun breakdownTable ->
                    div [_class "column"] [breakdownTable]
                )

        [
            div [_class "content"] [
                h1 [] [
                    str ("Screener: " + screener.name)
                ]
                h5 [] [ 
                    $"{results.Length} results" |> str
                    $" ({freshResults.Length} fresh hits)" |> str
                ]
                
                div [_class "block"] [
                    
                    generateHrefWithAttr
                        "View on Finviz"
                        screener.url
                        (_class "button is-primary mr-2")

                    generateHrefWithAttr
                        "Screener Details"
                        (screener.id |> Links.screenerLink)
                        (_class "button is-primary mr-2")

                    generateHrefWithAttrs
                        "NGTD Outcomes"
                        ((screener.name,tickers,tickersWithEarningsInResults,date) |> Links.ngtdOutcomesReportLink)
                        [(_class "button is-primary mr-2") ; (_target "_blank")]

                    button [
                        _onclick "toggleTickerCompanyVisibility()"
                        _class "button is-primary mr-2"
                        ] [
                        str "Toggle Ticker/Company"
                    ]

                    button [
                        _onclick "toggleEarningsVisibility()"
                        _class "button is-primary"
                        ] [
                        str "Toggle Earnings"
                    ]
                ]
            ]
            div [_class "columns"] breakdownDivs
            section [_class "content"] [
                h2 [] [str "Fresh Hits"]
                freshHitsTable
            ]
            section [_class "content"] [
                h2 [] [str "All Results"]
                screenerTable
            ]
        ]

    let handler ((id:int),(date:string))  = 
        
        // get screeners, render them in HTML
        let byIdOption = StockScreenerReports.Storage.Storage.getScreenerById id
        match byIdOption with
        | Some screener -> 
            let dateBefore = (Utils.subtractDaysToClosestBusinessDay (System.DateTime.Parse(date)) 1)

            let earningsTickers =
                date
                |> getTickersWithEarnings
                |> List.append (dateBefore |> Utils.convertToDateString |> getTickersWithEarnings)
                |> List.distinct

            let topGainers =
                match id with
                | Constants.TopGainerScreenerId -> []
                | _ -> date
                    |> getScreenerResults Constants.TopGainerScreenerId
                    |> List.map (fun r -> r.ticker)

            let previousHitsDateRange = (
                dateBefore.AddDays(-30) |> Utils.convertToDateString,
                dateBefore |> Utils.convertToDateString
            )

            let previousHits = id |> getTickersWithScreenerResultsForDateRange previousHitsDateRange |> Set.ofList


            let screenerResults = getScreenerResults screener.id date
            let view      = view screener screenerResults earningsTickers topGainers previousHits
            view |> mainLayout $"Screener: {screener.name}"
        | None ->
            notFound "Screener not found"