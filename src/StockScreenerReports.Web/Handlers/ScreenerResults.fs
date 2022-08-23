namespace StockScreenerReports.Web.Handlers

module ScreenerResults =

    open Giraffe.ViewEngine
    open StockScreenerReports.Storage.Reports
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Views
    open StockScreenerReports.Core

    let screenerResultToTr tickersWithEarnings (result:ScreenerResultReportItem) =
        
        let hasEarnings = tickersWithEarnings |> List.contains result.ticker

        let earningsIcon =
            match hasEarnings with
            | true -> i [_class "fa-solid fa-e"] []
            | false -> i [] []

        let rowAttributes = [_height "50px"]

        tr rowAttributes [
            result.date |> Utils.convertToDateString |> str |> toTdWithNode
            earningsIcon |> toTdWithNode
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

    let toBreakdownTable breakdownTitle linkFunction (breakdown:seq<string * list<ScreenerResultReportItem>>) =
        // row with headers for each column and another row with length
        let headerRow = tr [] [
            th [] [str breakdownTitle]
            th [] [str ""]
        ] 

        let valueRows =
            breakdown
            |> Seq.map (fun (name, list) ->
                tr [] [
                    td [] [
                        a [_href (linkFunction name)] [str name]
                    ]
                    td [] [str (list.Length.ToString())]
                ]
            )
            |> Seq.toList
        
        headerRow::valueRows |> fullWidthTable
        
    let calculateBreakdowns (screenerResults:list<ScreenerResultReportItem>) =
        
        let convertToBreakdown (name, linkFunction, groupByProperty) =
            (name, linkFunction, (screenerResults |> List.groupBy groupByProperty |> List.sortByDescending (fun (_, list) -> list.Length)))

        let breakdowns = [
            ("Sectors", Links.sectorLink, fun (a:ScreenerResultReportItem) -> a.sector);
            ("Industries", Links.industryLink, fun (a:ScreenerResultReportItem) -> a.industry);
            ("Countries", Links.countryLink, fun (a:ScreenerResultReportItem) -> a.country);
        ]
        
        breakdowns |> List.map convertToBreakdown

    let generateScreenerResultTable tickersWithEarnings results =
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
            |> List.map (fun r -> screenerResultToTr tickersWithEarnings r)
            |> List.append [tr [] headerCells]
            |> fullWidthTable

    let private view
        (screener:StockScreenerReports.Core.Screener)
        (results:list<ScreenerResultReportItem>)
        (tickersWithEarnings:list<string>) =

        let screenerTable = results |> generateScreenerResultTable tickersWithEarnings

        let breakdowns = calculateBreakdowns results
                
        let breakdownDivs = 
            breakdowns
            |> List.map (
                fun (breakdownTitle,linkFunction,breakdownList) ->
                    toBreakdownTable breakdownTitle linkFunction breakdownList
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
                
                div [_class "block"] [
                    
                    generateHrefWithAttr
                        "View on Finviz"
                        screener.url
                        (_class "button is-primary mr-2")

                    generateHrefWithAttr
                        "Screener Details"
                        (screener.id |> Links.screenerLink)
                        (_class "button is-primary")
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

            let screenerResults = getScreenerResults screener.id date
            let view      = view screener screenerResults earningsTickers
            view |> mainLayout $"Screener: {screener.name}"
        | None ->
            notFound "Screener not found"