namespace StockScreenerReports.Web.Handlers

module ScreenerResults =

    open Giraffe
    open Giraffe.ViewEngine
    open StockScreenerReports.Storage
    open StockScreenerReports.Storage.Reports
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Views
    open StockScreenerReports.Core
    open FSharp.Data

    type ScreenerExportType =   CsvProvider<
        Schema = "screenerid, date (string), ticker, name, sector (string), industry (string), country (string), marketCap (decimal), price (decimal), change (decimal), volume (decimal), url (string)",
        HasHeaders=false>

    let screenerResultToTr tickersWithEarnings topGainers (result:ScreenerResultReportItem) =
        
        let hasEarnings = tickersWithEarnings |> List.contains result.ticker
        let isTopGainer = topGainers |> List.contains result.ticker

        let earningsIcon = hasEarnings |> generateEarningsIcon
        let fireIcon = isTopGainer |> generateTopGainerIcon
            
        let rowAttributes = [_height "50px"]

        tr rowAttributes [
            TickerLinkColumn(result.ticker) |> toTd
            [earningsIcon; fireIcon] |> toTdWithNodes
            StringColumn(result.name) |> toTd 
            LinkColumn(result.sector, result.sector |> Links.sectorLink) |> toTd
            LinkColumn(result.industry, result.industry |> Links.industryLink) |> toTd
            LinkColumn(result.country, result.country |> Links.countryLink) |> toTd
            StringColumn(result.marketCap |> marketCapFormatted) |> toTd
            StringColumn(result.price |> dollarFormatted) |> toTd
            StringColumn(result.change |> percentFormatted) |> toTd
            StringColumn(result.volume |> volumeFormatted) |> toTd
            LinkNewTabColumn("chart", result.ticker |> Links.tradingViewLink) |> toTd
        ]
    
    let calculateBreakdowns (screenerResults:list<ScreenerResultReportItem>) =
        
        let convertToBreakdown (name, linkFunction, groupByProperty, clickFunction:ClickFunction) =
            (
                name,
                linkFunction,
                (screenerResults |> List.groupBy groupByProperty |> List.sortByDescending (fun (_, list) -> list.Length)) |> List.map (fun (key, list) -> (key, list.Length)),
                clickFunction
            )

        let breakdowns = [
            ("Sectors", Links.sectorLink, (fun (a:ScreenerResultReportItem) -> a.sector), SectorClicked);
            ("Industries", Links.industryLink, (fun (a:ScreenerResultReportItem) -> a.industry), IndustryClicked);
            ("Countries", Links.countryLink, (fun (a:ScreenerResultReportItem) -> a.country), CountryClicked);
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
            |> List.map (screenerResultToTr tickersWithEarnings topGainers)
            |> fullWidthTableWithSortableHeaderCells headers

    let private view
        (screener:Screener)
        (results:list<ScreenerResultReportItem>)
        (allTickersWithEarnings:list<string>)
        (topGainers:list<string>)
        (previousHits:Set<string>) =

        let screenerTable = results |> generateScreenerResultTable allTickersWithEarnings topGainers

        let freshResults = results |> List.filter (fun r -> not (previousHits |> Set.contains r.ticker))

        let freshHitsTable = freshResults |> generateScreenerResultTable allTickersWithEarnings topGainers

        let tickersWithEarningsInResults = results |> List.map _.ticker |> List.filter (fun t -> allTickersWithEarnings |> List.contains t)

        let breakdowns = calculateBreakdowns results

        let tickers = results |> List.map _.ticker
        
        let freshTickers = freshResults |> List.map _.ticker

        let date = results |> List.head |> _.date |> Utils.convertToDateString
                
        let breakdownDivs = 
            breakdowns
            |> List.map (
                fun (breakdownTitle,linkFunction,breakdownList, clickFunction) ->
                    toNameCountTableWithLinks breakdownTitle 10 linkFunction clickFunction breakdownList
                )
            |> List.map (
                fun breakdownTable ->
                    div [_class "column"] [breakdownTable]
                )

        let freshHitsTableSectionWithLink =
            section [_class "content"] [
                h4 [] [
                    "Fresh Hits" |> str
                    small [ _class "is-pulled-right"] [
                        generateHrefWithAttrs
                            "NGTD Trades"
                            ((screener.id,freshTickers) |> Links.ngtdTradesReportLink)
                            [(_class "button is-small is-primary mr-2"); (_target "_blank")]
                    ]
                    small [ _class "is-pulled-right mr-2"] [
                        generateHrefWithAttrs
                            "NGTD Outcomes"
                            ((screener.name,freshTickers,tickersWithEarningsInResults,"",date) |> Links.ngtdOutcomesReportLink)
                            [(_class "button is-small is-primary mr-2") ; (_target "_blank")]
                    ]
                ]
                freshHitsTable
            ]
        [
            div [_class "content"] [
                h1 [] [
                    $"Screener: {screener.name}" |> str
                ]
                h5 [] [ 
                    $"{results.Length} results on {date}" |> str
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
                        ((screener.name,tickers,tickersWithEarningsInResults,"",date) |> Links.ngtdOutcomesReportLink)
                        [(_class "button is-primary mr-2") ; (_target "_blank")]
                       
                    generateHrefWithAttrs
                        "NGTD Trades"
                        ((screener.id,tickers) |> Links.ngtdTradesReportLink)
                        [(_class "button is-primary mr-2"); (_target "_blank")]

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
            freshHitsTableSectionWithLink
            screenerTable |> toSection "All Results"
        ]

    let exportHandler id =
        setHttpHeader "Content-Type" "text/csv"
        >=> 
            let screener = Storage.getScreenerById id
            let filename =
                match screener with
                | Some s -> $"export_{s.name}.csv"
                | None -> "export.csv"

            let escapedFilename = System.Uri.EscapeDataString(filename)

            setHttpHeader "Content-Disposition" $"attachment; filename={escapedFilename}"
        >=>
            let data = getAllScreenerResults id
            let rows = 
                data 
                |> List.map (fun r -> 
                    ScreenerExportType.Row(
                        r.screenerid.ToString(),
                        r.date |> Utils.convertToDateString,
                        r.ticker,
                        r.name,
                        r.sector,
                        r.industry,
                        r.country,
                        r.marketCap,
                        r.price,
                        r.change,
                        r.volume,
                        r.ticker |> Links.tradingViewLink
                    )
                )

            (new ScreenerExportType(rows)).SaveToString()
            |> setBodyFromString
            
    let handler (id:int,dateStr:string)  = 
        
        // get screeners, render them in HTML
        let byIdOption = Storage.getScreenerById id
        match byIdOption with
        | Some screener -> 
            let date = 
                match dateStr with
                | "latest" -> 
                    getLatestScreenerResults()
                    |> List.filter (fun r -> r.screenerid = id)
                    |> List.head
                    |> (fun r -> r.date)
                | _ -> System.DateTime.Parse(dateStr)
                    
            let dateBefore = (Utils.subtractDaysToClosestBusinessDay date 1)

            let earningsTickers =
                date
                |> Utils.convertToDateString
                |> getTickersWithEarnings
                |> List.append (dateBefore |> Utils.convertToDateString |> getTickersWithEarnings)
                |> List.distinct

            let topGainers =
                match id with
                | Constants.TopGainerScreenerId -> []
                | _ -> date
                    |> Utils.convertToDateString
                    |> getScreenerResults Constants.TopGainerScreenerId
                    |> List.map (fun r -> r.ticker)

            let previousHitsDateRange = (
                dateBefore.AddDays(-30) |> Utils.convertToDateString,
                dateBefore |> Utils.convertToDateString
            )

            let previousHits = id |> getTickersWithScreenerResultsForDateRange previousHitsDateRange |> Set.ofList


            let screenerResults = date |> Utils.convertToDateString |> getScreenerResults screener.id
            let view      = view screener screenerResults earningsTickers topGainers previousHits
            view |> mainLayout $"Screener: {screener.name}"
        | None ->
            notFound "Screener not found"
