namespace StockScreenerReports.Web.Handlers

open System

module Earnings =
    open Giraffe.ViewEngine.Attributes
    open Giraffe.ViewEngine.HtmlElements
    open StockScreenerReports.Core
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Views
    open StockScreenerReports.Storage.Reports

    let private createMapFromScreenerResults (stocks:ScreenerResultReportItem list) =
        stocks
        |> List.map (fun s -> (s.ticker, s.date) , s)
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
    
    let private createIndustryGrouping stockDatePairs (membershipMap:Map<string * DateTime,ScreenerResultReportItem>) =
        stockDatePairs
        |> List.where (fun stockAndDate ->
            match membershipMap.ContainsKey stockAndDate with
            | true ->
                let screenerResult = membershipMap[stockAndDate]
                screenerResult.date = (stockAndDate |> snd)
            | false -> false
        )
        |> List.map (fun stockAndDate -> membershipMap[stockAndDate])
        |> List.groupBy (_.industry)

    let private createNameCountTableColumnDiv title grouping =
        let withCounts =
            grouping
            |> List.map (fun (i,l) -> (i, l |> List.length))
            |> List.sortByDescending snd
        
        let countOfTickers = withCounts |> List.sumBy snd

        let newTitle = $"{title} ({countOfTickers})"

        let header, rows = 
            withCounts |>
            toHeaderAndRowsWithLinksAndClickFunc 
                newTitle
                10
                (fun s -> s |> Links.industryLink)
                (Some "industryClicked(event); highlightDashboardIndustry(event)")

        let table = fullWidthTableWithCssClass "dashboard-industry-table" header rows

        div [_class "column"] [table]

    let private createFilteredSection title (matchFilter:Map<string * DateTime,ScreenerResultReportItem>) tickersWithEarnings =
        let rows = 
            tickersWithEarnings
            |> List.filter (fun tickerDate -> tickerDate |> matchFilter.ContainsKey)
            |> List.map (fun tickerDate -> matchFilter[tickerDate])
            |> List.sortBy _.industry
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
        (tickersWithEarnings:list<string * DateTime>)
        (screenerResultMappings:list<Map<string * DateTime,ScreenerResultReportItem>>) =
        let rows =
            tickersWithEarnings
            |> List.map (fun (ticker, date:DateTime) ->

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
                        NodeColumn((ticker,date) |> map.TryFind |> generateDivWithDateAndIcon)
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
                        yield! screenerCells
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

        let tickersWithEarnings =
            Storage.getEarningsTickers dateRange
            |> List.map (fun (ticker,date,earningsTime) ->
                let effectiveDate =
                    match earningsTime with
                    | BeforeMarket -> date
                    | AfterMarket -> date.AddDays(1)
                ticker, effectiveDate)
        
        let stocks =
            tickersWithEarnings
            |> List.map fst
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