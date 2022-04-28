namespace FinvizScraper.Web.Handlers

module IndustryDashboard =
    open FinvizScraper.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open FinvizScraper.Storage
    open Giraffe.ViewEngine.Attributes
    open FinvizScraper.Web.Shared.Views

    let handler industryName =
        
        let createTrendSpan (trend:option<FinvizScraper.Core.IndustryUpdate>) =
            let desc = 
                match trend with
                | None -> "No 20 SMA trends found"
                | Some t -> 
                    let total = t.above + t.below
                    let pct = System.Math.Round((double t.above) * 100.0 / (double total), 2)
                    $"<b>{pct}%%</b> ({t.above}) above {t.days} SMA"

            span [ _class "mx-1"] [
                rawText desc
            ]
            
        // load industry trends
        let trendsDiv = div [] [
            (industryName |> Storage.getMostRecentIndustryTrends 20 |> createTrendSpan)
            (industryName |> Storage.getMostRecentIndustryTrends 200 |> createTrendSpan)
        ]
        
        // load charts for each screener
        let screeners = Storage.getScreeners()

        let days = FinvizScraper.Core.FinvizConfig.dayRange

        let list = [for i in -days .. 0 -> (System.DateTime.UtcNow.Date.AddDays(i),0) ]

        let charts = 
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
                |> Charts.convertNameCountsToChart screener.name Charts.smallChart
                |> div [_class "block"] 
            )

        let stocks = Storage.getStocksByIndustry industryName

        let stockTable =
            stocks
            |> List.map (fun stock ->
                tr [] [
                    td [] [
                        stock.ticker |> FinvizScraper.Core.StockTicker.value |> generateTickerLink
                    ]
                    td [] [str stock.company]
                    td [] [ generateHref stock.sector (Links.sectorLink stock.sector) ]
                    td [] [ generateHref stock.industry (Links.industryLink stock.industry) ]
                    td [] [ stock.ticker |> FinvizScraper.Core.StockTicker.value |> Links.tradingViewLink |> generateHref "Trading View" ]
                ]
            )
            |> fullWidthTable

        let view = 
            div [_class "content"] [
                h1 [] [
                    str industryName
                ]
                trendsDiv
            ]::charts @ [stockTable]
        
        view |> Views.mainLayout $"Industry Dashboard for {industryName}" 