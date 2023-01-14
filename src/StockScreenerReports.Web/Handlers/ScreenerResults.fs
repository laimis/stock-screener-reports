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
            result.date |> Utils.convertToDateString |> str |> toTdWithNode
            [earningsIcon; fireIcon] |> toTdWithNodes
            result.ticker |> generateTickerLink |> toTdWithNode
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
            "Date"
            "" 
            "Ticker"
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

        let headerCells = headers |> List.map toSortableHeaderCell

        results
            |> List.map (fun r -> screenerResultToTr tickersWithEarnings topGainers r)
            |> List.append [tr [] headerCells]
            |> fullWidthTable

    let private view
        (screener:StockScreenerReports.Core.Screener)
        (results:list<ScreenerResultReportItem>)
        (tickersWithEarnings:list<string>)
        (topGainers:list<string>) =

        let screenerTable = results |> generateScreenerResultTable tickersWithEarnings topGainers

        let breakdowns = calculateBreakdowns results

        let tickers = results |> List.map (fun r -> r.ticker)
                
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
                        (tickers |> Links.ngtdOutcomesReportLink)
                        [(_class "button is-primary mr-2") ; (_target "_blank")]

                    button [
                        _onclick "toggleTickerCompanyVisibility()"
                        _class "button is-primary"
                        ] [
                        str "Toggle Ticker/Company"
                    ]
                ]
            ]
            div [_class "columns"] breakdownDivs
            div [_class "block"] [screenerTable]
        ]

    let handler ((id:int),(date:string))  = 
        
        // get screeners, render them in HTML
        let byIdOption = StockScreenerReports.Storage.Storage.getScreenerById id
        match byIdOption with
        | Some screener -> 
            let dateBefore = (Utils.subtractDaysToClosestBusinessDay (System.DateTime.Parse(date)) 1) |> Utils.convertToDateString

            let earningsTickers =
                date
                |> getTickersWithEarnings
                |> List.append (dateBefore |> getTickersWithEarnings)
                |> List.distinct

            let topGainers =
                match id with
                | Constants.TopGainerScreenerId -> []
                | _ -> date
                    |> getScreenerResults Constants.TopGainerScreenerId
                    |> List.map (fun r -> r.ticker)

            let screenerResults = getScreenerResults screener.id date
            let view      = view screener screenerResults earningsTickers topGainers
            view |> mainLayout $"Screener: {screener.name}"
        | None ->
            notFound "Screener not found"