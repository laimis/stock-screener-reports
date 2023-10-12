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

    let private createEarningsByDateChart dateCountList =

        let data = dateCountList |> List.map (fun (_,count) -> count)
        let labels = dateCountList |> List.map (fun (date,_) -> date |> Utils.convertToDateString)

        let dataset:Charts.DataSet<int> = {
                    data = data
                    title = "Earnings By Date"
                    color = ReportsConfig.getBackgroundColorDefault
                }

        let chart = div [] (Charts.generateChartElements "Earnings By Date" Charts.Bar None Charts.smallChart labels [dataset])

        chart |> toSection "Earnings By Date"
    
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

        let (header, rows) = 
            withCounts |>
            toHeaderAndRowsWithLinksAndClickFunc 
                newTitle
                10
                (fun s -> s |> Links.industryLink)
                (Some "industryClicked(event); highlightDashboardIndustry(event)")

        let table = fullWidthTableWithCssClass "dashboard-industry-table" header rows

        div [_class "column"] [table]

    let private createFilteredSection title (matchFilter:Map<string,ScreenerResultReportItem>) tickersWithEarnings =
        let rows = 
            tickersWithEarnings
            |> List.filter (fun (ticker,_) -> ticker |> matchFilter.ContainsKey)
            |> List.map (fun (ticker, _) -> matchFilter.[ticker])
            |> List.sortBy (fun s -> s.industry)
            |> List.map (fun s -> 
                [
                    TickerLinkColumn(s.ticker)
                    LinkColumn(s.sector, s.sector |> Links.sectorLink)
                    LinkColumn(s.industry, s.industry |> Links.industryLink)
                    LinkNewTabColumn("chart", s.ticker |> Links.tradingViewLink)
                ] |> toTr
            )

        let headerRow = ["Ticker"; "Chart"; "Industry"; "Sector"]

        let table =
            match rows with
            | [] -> p [] [str "No results"]
            | _ -> rows |> fullWidthTableWithSortableHeaderCells headerRow

        table |> toSection title

    let createEarningsTable
        (stocks:Map<string,Stock>)
        tickersWithEarnings
        (screenerResultMappings:list<Map<string,ScreenerResultReportItem>>) =
        let rows =
            tickersWithEarnings
            |> List.map (fun (ticker, (date:System.DateTime)) ->

                let iconFunc screenerId =
                    match screenerId with
                    | Constants.NewHighsScreenerId -> generateNewHighIcon
                    | Constants.TopGainerScreenerId -> generateTopGainerIcon
                    | Constants.TopLoserScreenerId -> generateTopLoserIcon
                    | Constants.NewLowsScreenerId -> generateNewLowIcon
                    | _ -> failwith "No icon defined for screener id"

                let generateDivWithDateAndIcon (screenerResultOption:ScreenerResultReportItem option) =
                    match screenerResultOption with
                    | Some s ->
                        div [] [
                            true |> (s.screenerid |> iconFunc)
                            s.date |> Utils.convertToDateString |> str
                        ]
                    | None -> 
                        i [] []

                let screenerCells =
                    screenerResultMappings
                    |> List.map (fun map -> 
                        NodeColumn(ticker |> map.TryFind |> generateDivWithDateAndIcon)
                    )

                let stock = stocks |> Map.tryFind ticker
                let industry = 
                    match stock with
                    | Some s -> s.industry
                    | None -> ""

                let marketCap = 
                    match stock with
                    | Some s -> s.marketCap
                    | None -> None

                let cells = 
                    [
                        TickerLinkColumn(ticker)
                        DateColumn(date)
                        LinkColumn(industry, industry |> Links.industryLink)
                        StringColumn(marketCap |> marketCapOptionFormatted)
                    ]
                    @
                    screenerCells
                    @
                    [
                        LinkNewTabColumn("chart", ticker |> Links.tradingViewLink)
                    ]

                cells |> toTr
            )
 
        let headerRow = ["Ticker"; "Date"; "Industry"; "MCap"; "New High"; "Top Gainer"; "Top Loser"; "New Low"; "Trading View"]
        rows |> fullWidthTableWithSortableHeaderCells headerRow
            

    let handlerInternal startDate endDate  =
        
        let header = div [_class "content"] [
            h1 [] [str "Earnings"]
        ]

        let dateRange = (
            startDate |> Utils.convertToDateString,
            endDate |> Utils.convertToDateString
        )

        let tickersWithEarnings = getEarningsTickers dateRange
        let stocks =
            tickersWithEarnings
            |> List.map (fun (ticker,_) -> ticker)
            |> Storage.getStockByTickers
            |> List.map (fun s -> s.ticker |> StockTicker.value, s)
            |> Map.ofList

        let screenerResultMappings =
            [Constants.NewHighsScreenerId; Constants.TopGainerScreenerId; Constants.TopLoserScreenerId; Constants.NewLowsScreenerId]
            |> List.map (fun id -> 
                id |> getScreenerResultsForDays dateRange |> createMapFromScreenerResults)

        let industryGroupingsByScreener =
            screenerResultMappings
            |> List.map (fun map -> map |> createIndustryGrouping tickersWithEarnings)

        let historicalDateRange = (
            ReportsConfig.now().AddDays(-30) |> Utils.convertToDateString,
            ReportsConfig.now() |> Utils.convertToDateString
        )
        
        let earningsByDate = historicalDateRange |> getEarningCountByDate

        let earningChart = earningsByDate |> createEarningsByDateChart
        
        let screenerLabels = [
            "New Highs"
            "Top Gainers"
            "Top Losers"
            "New Lows"
        ]

        let divs =
            screenerLabels
            |> List.indexed
            |> List.map(fun (i,l) -> 
                let grouping = industryGroupingsByScreener[i]
                grouping |> createNameCountTableColumnDiv l
            )

        let breakdownDiv = div [_class "columns"] divs

        let sections =
            screenerLabels
            |> List.indexed
            |> List.map (fun (i,l) ->
                let map = screenerResultMappings[i]
                tickersWithEarnings |> createFilteredSection l map
            )

        let earningsTable = createEarningsTable stocks tickersWithEarnings screenerResultMappings

        [header; breakdownDiv] @ [earningsTable] @ sections @ [earningChart] |> mainLayout $"Earnings"

    let handlerCurrentWeek() =
        let startDate = Utils.getCurrentMonday()
        let endDate = startDate.AddDays(6)
        handlerInternal startDate endDate

    let handlerLast7Days() =
        let startDate = ReportsConfig.now().AddDays(-7)
        let endDate = ReportsConfig.now().AddDays(1)

        handlerInternal startDate endDate