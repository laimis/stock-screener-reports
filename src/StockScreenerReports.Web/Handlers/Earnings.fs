namespace StockScreenerReports.Web.Handlers

module Earnings =
    open Giraffe.ViewEngine.Attributes
    open Giraffe.ViewEngine.HtmlElements
    open StockScreenerReports.Core
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Views
    open StockScreenerReports.Storage.Reports

    let private createMapFromScreenerResults stocks =
        stocks
        |> List.map (fun s -> s.ticker, s)
        |> Map.ofList
    
    let private createIndustryGrouping stockDatePairs (membershipMap:Map<string,ScreenerResultReportItem>) =
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
            |> toNameCountTableWithLinksAndClickFunc 
                newTitle
                10
                (fun s -> s |> Links.industryLink)
                (Some "industryClicked(event)")

        div [_class "column"] [table]

    let private createFilteredSection title (matchFilter:Map<string,ScreenerResultReportItem>) tickersWithEarnings =
        let rows = 
            tickersWithEarnings
            |> List.filter (fun (ticker,_) -> ticker |> matchFilter.ContainsKey)
            |> List.map (fun (ticker, _) -> matchFilter.[ticker])
            |> List.sortBy (fun s -> s.industry)
            |> List.map (fun s -> 
                tr [] [
                    s.ticker |> generateTickerLink |> toTdWithNode
                    s.ticker |> Links.tradingViewLink |> generateHref "chart" |> toTdWithNode
                    s.industry |> Links.industryLink |> generateHref s.industry |> toTdWithNode
                    s.sector |> Links.sectorLink |> generateHref s.sector |> toTdWithNode
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

        let tickersWithEarnings = getEarningsTickers startDate endDate
        let stocks =
            tickersWithEarnings
            |> List.map (fun (ticker,_) -> ticker)
            |> Storage.getStockByTickers
            |> List.map (fun s -> s.ticker |> StockTicker.value, s)
            |> Map.ofList

        let dateRange = (
            startDate |> Utils.convertToDateStringForOffset,
            endDate |> Utils.convertToDateStringForOffset
        )

        let newHighs =
            Constants.NewHighsScreenerId
            |> getScreenerResultsForDays dateRange
        let newHighsMap = newHighs |> createMapFromScreenerResults
        let newHighIndustries = createIndustryGrouping tickersWithEarnings newHighsMap

        let topGainers =
            Constants.TopGainerScreenerId
            |> getScreenerResultsForDays dateRange
        let topGainersMap = topGainers |> createMapFromScreenerResults
        let topGainerIndustries = createIndustryGrouping tickersWithEarnings topGainersMap

        let topLosers =
            Constants.TopLoserScreenerId
            |> getScreenerResultsForDays dateRange
        let topLosersMap = topLosers |> createMapFromScreenerResults
        let topLoserIndustries = createIndustryGrouping tickersWithEarnings topLosersMap

        let newLows =
            Constants.NewLowsScreenerId
            |> getScreenerResultsForDays dateRange
        let newLowsMap = newLows |> createMapFromScreenerResults
        let newLowIndustries = createIndustryGrouping tickersWithEarnings newLowsMap

        let rows =
            tickersWithEarnings
            |> List.map (fun (ticker, date) ->

                let generateDivWithDateAndIcon iconFunc (screenerResultOption:ScreenerResultReportItem option) =
                    match screenerResultOption with
                    | Some s ->
                        div [] [
                            true |> iconFunc
                            s.date |> Utils.convertToDateString |> str
                        ]
                    | None -> 
                        false |> iconFunc

                let newHighs = ticker |> newHighsMap.TryFind |> generateDivWithDateAndIcon generateNewHighIcon
                let topGainers = ticker |> topGainersMap.TryFind |> generateDivWithDateAndIcon generateTopGainerIcon
                let topLosers = ticker |> topLosersMap.TryFind |> generateDivWithDateAndIcon generateTopLoserIcon
                let newLows = ticker |> newLowsMap.TryFind |> generateDivWithDateAndIcon generateNewLowIcon

                let stock = stocks |> Map.tryFind ticker
                let industry = 
                    match stock with
                    | Some s -> s.industry
                    | None -> ""

                tr [] [
                    ticker |> generateTickerLink |> toTdWithNode
                    date.ToString("yyyy-MM-dd") |> toTd
                    industry |> Links.industryLink |> generateHref industry |> toTdWithNode
                    newHighs |> toTdWithNode
                    topGainers |> toTdWithNode
                    topLosers |> toTdWithNode
                    newLows |> toTdWithNode
                    ticker |> Links.tradingViewLink |> generateHrefNewTab "chart" |> toTdWithNode
                ]
            )

        let earningsTable = 
            let headerRow = ["Ticker"; "Date"; "Industry"; "New High"; "Top Gainer"; "Top Loser"; "New Low"; "Trading View"]
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