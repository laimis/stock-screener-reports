namespace StockScreenerReports.Web.Handlers

module Earnings =
    open StockScreenerReports.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open Giraffe.ViewEngine.Attributes
    open StockScreenerReports.Storage
    open StockScreenerReports.Core

    let private createMapFromStocks stocks =
        stocks
        |> List.map (fun s -> s.ticker |> StockTicker.value, s)
        |> Map.ofList
    
    let private createIndustryGrouping stockDatePairs (membershipMap:Map<string,Stock>) =
        stockDatePairs
        |> List.map (fun pair -> 
            let ticker,_ = pair
            ticker
        )
        |> List.where (fun t -> membershipMap.ContainsKey t)
        |> List.map (fun t -> membershipMap.[t])
        |> List.groupBy (fun s -> s.industry)

    let private createNameCountTableColumnDiv title grouping =
        let withCounts =
            grouping
            |> List.map (fun (i,l) -> (i, l |> List.length))
            |> List.sortByDescending (fun (_,c) -> c)
        
        let table = 
            withCounts
            |> Views.toNameCountTableWithLinks title 10 (fun s -> s |> Links.industryLink)

        div [_class "column"] [table]

    let private createFilteredSection title (matchFilter:Map<string,Stock>) tickersWithEarnings =
        let rows = 
            tickersWithEarnings
            |> List.filter (fun (ticker,_) -> ticker |> matchFilter.ContainsKey)
            |> List.map (fun (ticker, date) -> matchFilter.[ticker])
            |> List.sortBy (fun s -> s.industry)
            |> List.map (fun s -> 
                tr [] [
                    td [] [s.ticker |> StockTicker.value |> Views.generateTickerLink]
                    td [] [s.industry |> str]
                    td [] [s.sector |> str]
                ]
            )

        let headerRow = tr [] (["Ticker"; "Industry"; "Sector"] |> List.map (fun s -> s |> Views.toSortableHeaderCell))

        let table = headerRow::rows |> Views.fullWidthTable

        div [ _class "content"] [
            h2 [] [title |> str]
            table
        ]
            

    let handler()  =
        
        let header = div [_class "content"] [
            h1 [] [str "Earnings"]
        ]

        let startDate = System.DateTimeOffset.UtcNow.AddDays(-7)
        let endDate = System.DateTimeOffset.UtcNow.AddDays(1)

        let tickersWithEarnings = Reports.getEarningsTickers startDate endDate

        let newHighs =
            Constants.NewHighsScreenerId
            |> Reports.getStocksForScreenerAndDates startDate endDate
        let newHighsMap = newHighs |> createMapFromStocks
        let newHighIndustries = createIndustryGrouping tickersWithEarnings newHighsMap

        let topGainers =
            Constants.TopGainerScreenerId
            |> Reports.getStocksForScreenerAndDates  startDate endDate
        let topGainersMap = topGainers |> createMapFromStocks
        let topGainerIndustries = createIndustryGrouping tickersWithEarnings topGainersMap

        let topLosers =
            Constants.TopLoserScreenerId
            |> Reports.getStocksForScreenerAndDates startDate endDate
        let topLosersMap = topLosers |> createMapFromStocks
        let topLoserIndustries = createIndustryGrouping tickersWithEarnings topLosersMap

        let newLows =
            Constants.NewLowsScreenerId
            |> Reports.getStocksForScreenerAndDates startDate endDate
        let newLowsMap = newLows |> createMapFromStocks
        let newLowIndustries = createIndustryGrouping tickersWithEarnings newLowsMap

        let rows =
            tickersWithEarnings
            |> List.map (fun (ticker, date) ->

                let newHighs = ticker |> newHighsMap.ContainsKey |> Views.generateNewHighIcon
                let topGainers = ticker |> topGainersMap.ContainsKey |> Views.generateTopGainerIcon
                let topLosers = ticker |> topLosersMap.ContainsKey |> Views.generateTopLoserIcon
                let newLows = ticker |> newLowsMap.ContainsKey |> Views.generateNewLowIcon

                tr [] [
                    td [] [ticker |> Views.generateTickerLink]
                    td [] [date.ToString("yyyy-MM-dd") |> str]
                    td [] [newHighs]
                    td [] [topGainers]
                    td [] [topLosers]
                    td [] [newLows]
                    td [] [ticker |> Links.tradingViewLink |> Views.generateHrefNewTab "link"]
                ]
            )

        let earningsTable = 
            table [_class "table is-striped is-fullwidth"] [
                thead [] [
                    tr [] [
                        th [] [str "Ticker"]
                        th [] [str "Date"]
                        th [] [str "New High"]
                        th [] [str "Top Gainer"]
                        th [] [str "Top Loser"]
                        th [] [str "New Low"]
                        th [] [str "Trading View"]
                    ]
                ]
                tbody [] rows
            ]
        
        // break down div
        let breakdownDiv = div [_class "columns"] [
            newHighIndustries |> createNameCountTableColumnDiv "New Highs" 
            topGainerIndustries |> createNameCountTableColumnDiv "Top Gainers" 
            topLoserIndustries |> createNameCountTableColumnDiv "Top Losers" 
            newLowIndustries |> createNameCountTableColumnDiv "New Lows" 
        ]

        let newHighsSection = tickersWithEarnings |> createFilteredSection "New Highs" newHighsMap
        let topGainersSection = tickersWithEarnings |> createFilteredSection "Top Gainers" topGainersMap
        let topLosersSection = tickersWithEarnings |> createFilteredSection "Top Losers" topLosersMap
        let newLowsSection = tickersWithEarnings |> createFilteredSection "New Lows" newLowsMap
        
        [header; breakdownDiv; newHighsSection; topGainersSection; topLosersSection; newLowsSection; earningsTable] |> Views.mainLayout $"Earnings"