namespace FinvizScraper.Web.Handlers

module IndustryDashboard =
    open FinvizScraper.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open FinvizScraper.Storage
    open FinvizScraper.Core
    open Giraffe.ViewEngine.Attributes
    open FinvizScraper.Web.Shared.Views

    let handler industryName =
        
        // load industry trends
        let createTrendSpan (trend:option<FinvizScraper.Core.IndustrySMABreakdown>) =
            let desc = 
                match trend with
                | None -> "No 20 SMA trends found"
                | Some t -> 
                    let total = t.breakdown.above + t.breakdown.below
                    let pct = System.Math.Round((double t.breakdown.above) * 100.0 / (double total), 2)
                    $"<b>{pct}%%</b> ({t.breakdown.above} / {total}) above {t.breakdown.days} SMA"

            span [ _class "mx-1"] [
                rawText desc
            ]
            
        let trendsDiv = div [] [
            (industryName |> Reports.getMostRecentIndustrySMABreakdown 20 |> createTrendSpan)
            (industryName |> Reports.getMostRecentIndustrySMABreakdown 200 |> createTrendSpan)
            span [ _class "mx-1"] [industryName |> Links.industryFinvizLink |> generateHrefNewTab "Finviz"]
        ]

        let industryTrendCharts =
            let industryTrendChartsInternal days =
                industryName
                |> Reports.getIndustrySMABreakdownsForIndustry days
                |> List.map (fun u -> (u.breakdown.date,System.Math.Round(u.breakdown.percentAbove, 0)))
                |> Charts.convertNameCountsToChart $"{days} EMA Trend" Charts.Line (Some 100) Charts.smallChart FinvizConfig.getBackgroundColorDefault 

            (industryTrendChartsInternal 20) @ (industryTrendChartsInternal 200)
        
        // load charts for each screener
        let screeners = Storage.getScreeners()

        let days = FinvizConfig.dayRange

        let list = days |> Logic.businessDatesWithZeroPairs

        let screenerCharts = 
            screeners
            |> List.map (fun screener ->
                let data = Reports.getDailyCountsForScreenerAndIndustry screener.id industryName days

                let mapped = data |> Map.ofList

                list
                |> List.map(fun (date,count) ->
                    let found = mapped.TryFind date
                    match found with
                    | Some c -> (date,c)
                    | None -> (date,count)
                )
                |> Charts.convertNameCountsToChart screener.name Charts.Bar None Charts.smallChart (FinvizConfig.getBackgroundColorForScreenerId screener.id)
                |> div [_class "block"] 
            )

        let resultRows =
            industryName
            |> Reports.getScreenerResultsForIndustry 50
            |> List.map (fun screenerResult ->
                tr [] [
                    td [] [ screenerResult.date.ToString("yyyy-MM-dd") |> str ]
                    td [] [
                        (screenerResult.screenerid,screenerResult.screenername) |> Views.generateScreenerTags
                    ]
                    td [] [ screenerResult.ticker |> generateTickerLink ]
                    td [] [ screenerResult.marketCap |> marketCapFormatted |> str ]
                    td [] [ screenerResult.price |> dollarFormatted |> str ]
                    td [] [ screenerResult.change |> percentFormatted |> str ]
                    td [] [ screenerResult.volume |> volumeFormatted |> str ]
                    td [] [ screenerResult.ticker |> Links.tradingViewLink |> generateHrefNewTab "chart" ]
                ]
            )

        let tableHeader =
            tr [] [
                th [] [str "date"]
                th [] [str "screener"]
                th [] [str "ticker"]
                th [] [str "market cap"]
                th [] [str "price"]
                th [] [str "change"]
                th [] [str "volume"]
                th [] [str "trading view"]
            ]

        let screenerResultsTable = tableHeader::resultRows |> Views.fullWidthTable
        
        // get stocks in industry
        let stocks = Storage.getStocksByIndustry industryName

        let stockTable =
            stocks
            |> List.map (fun stock ->
                tr [] [
                    td [] [
                        stock.ticker |> StockTicker.value |> generateTickerLink
                    ]
                    td [] [str stock.company]
                    td [] [ generateHref stock.sector (Links.sectorLink stock.sector) ]
                    td [] [ generateHref stock.industry (Links.industryLink stock.industry) ]
                    td [] [ stock.ticker |> StockTicker.value |> Links.tradingViewLink |> generateHref "Trading View" ]
                ]
            )
            |> fullWidthTable

        let view = 
            div [_class "content"] [
                h1 [] [
                    str industryName
                ]
                trendsDiv
            ]::industryTrendCharts @ screenerCharts @ [screenerResultsTable; stockTable]
        
        view |> mainLayout $"Industry Dashboard for {industryName}" 