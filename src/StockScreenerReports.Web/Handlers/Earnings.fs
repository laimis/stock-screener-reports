namespace StockScreenerReports.Web.Handlers

module Earnings =
    open Giraffe.ViewEngine.Attributes
    open Giraffe.ViewEngine.HtmlElements
    open StockScreenerReports.Core
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Views

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
        
        let countOfTickers = withCounts |> List.sumBy (fun (_,c) -> c)

        let newTitle = $"{title} ({countOfTickers})"

        let table = 
            withCounts
            |> toNameCountTableWithLinks newTitle 10 (fun s -> s |> Links.industryLink)

        div [_class "column"] [table]

    let private createFilteredSection title (matchFilter:Map<string,Stock>) tickersWithEarnings =
        let rows = 
            tickersWithEarnings
            |> List.filter (fun (ticker,_) -> ticker |> matchFilter.ContainsKey)
            |> List.map (fun (ticker, _) -> matchFilter.[ticker])
            |> List.sortBy (fun s -> s.industry)
            |> List.map (fun s -> 
                tr [] [
                    s.ticker |> StockTicker.value |> generateTickerLink |> toTdWithNode
                    s.ticker |> StockTicker.value |> Links.tradingViewLink |> generateHref "chart" |> toTdWithNode
                    s.industry |> str |> toTdWithNode
                    s.sector |> str |> toTdWithNode
                ]
            )

        let headerRow = ["Ticker"; "Chart"; "Industry"; "Sector"]

        let table = rows |> fullWidthTableWithSortableHeaderCells headerRow

        div [ _class "content"] [
            h2 [] [title |> str]
            table
        ]
            

    let handlerInternal startDate endDate  =
        
        let header = div [_class "content"] [
            h1 [] [str "Earnings"]
        ]

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

                let newHighs = ticker |> newHighsMap.ContainsKey |> generateNewHighIcon
                let topGainers = ticker |> topGainersMap.ContainsKey |> generateTopGainerIcon
                let topLosers = ticker |> topLosersMap.ContainsKey |> generateTopLoserIcon
                let newLows = ticker |> newLowsMap.ContainsKey |> generateNewLowIcon

                tr [] [
                    ticker |> generateTickerLink |> toTdWithNode
                    date.ToString("yyyy-MM-dd") |> toTd
                    newHighs |> toTdWithNode
                    topGainers |> toTdWithNode
                    topLosers |> toTdWithNode
                    newLows |> toTdWithNode
                    ticker |> Links.tradingViewLink |> generateHrefNewTab "chart" |> toTdWithNode
                ]
            )

        let earningsTable = 
            let headerRow = ["Ticker"; "Date"; "New High"; "Top Gainer"; "Top Loser"; "New Low"; "Trading View"]
            rows |> fullWidthTable headerRow
        
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
        
        [header; breakdownDiv; newHighsSection; topGainersSection; topLosersSection; newLowsSection; earningsTable] |> mainLayout $"Earnings"

    let handlerCurrentWeek() =
        let startDate = Utils.getCurrentMonday()
        let endDate = startDate.AddDays(6)
        handlerInternal startDate endDate

    let handlerLast7Days() =
        let startDate = System.DateTimeOffset.UtcNow.AddDays(-7)
        let endDate = System.DateTimeOffset.UtcNow.AddDays(1)

        handlerInternal startDate endDate