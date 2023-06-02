namespace StockScreenerReports.Web.Handlers

module SectorDashboard =
    open StockScreenerReports.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open StockScreenerReports.Storage
    open Giraffe.ViewEngine.Attributes
    open StockScreenerReports.Web.Shared.Views
    open StockScreenerReports.Core

    let handler sectorName =
        let screeners = Storage.getScreeners()

        let dateRange = ReportsConfig.dateRange
        let dateRangeAsStrings = ReportsConfig.dateRangeAsStrings

        let list = dateRange |> ReportsConfig.listOfBusinessDates

        let charts = 
            screeners
            |> List.map (fun screener ->
                let data = Reports.getDailyCountsForScreenerAndSector screener.id sectorName dateRangeAsStrings

                let mapped = data |> Map.ofList

                list
                |> Seq.map(fun (date) ->
                    let found = mapped.TryFind date
                    match found with
                    | Some c -> (date,c)
                    | None -> (date,0)
                )
                |> Charts.convertNameCountsToChart screener.name Charts.Bar None Charts.smallChart (ReportsConfig.getBackgroundColorForScreenerId screener.id)
                |> div [_class "block"] 
            )

        let resultRows =
            sectorName
            |> Reports.getScreenerResultsForSector 50
            |> List.map (fun screenerResult ->
                tr [] [
                    screenerResult.date |> Utils.convertToDateString |> toTd
                    (screenerResult.screenerid,screenerResult.screenername) |> generateScreenerTags |> toTdWithNode
                    screenerResult.ticker |> generateTickerLink |> toTdWithNode
                    screenerResult.marketCap |> marketCapFormatted |> toTd
                    screenerResult.price |> dollarFormatted |> toTd
                    screenerResult.change |> percentFormatted |> toTd
                    screenerResult.volume |> volumeFormatted |> toTd
                    screenerResult.ticker |> Links.tradingViewLink |> generateHref "chart" |> toTdWithNode
                ]
            )

        let headerCells = [
            "Date"
            "Screener"
            "Ticker"
            "Market Cap"
            "Price"
            "Change"
            "Volume"
            "Trading View"
        ]

        let screenerResultsTable = resultRows |> fullWidthTableWithSortableHeaderCells headerCells

        let stocks = sectorName |> Storage.getStocksBySector
        let stockTable = stocks |> generateStockTable

        let stocksSection = section [_class "mt-5"] [
            h4 [] [
                $"Stocks in sector ({stocks.Length})" |> str
            ]
            stockTable
        ]

        let contentSections =
            [
                [
                    h1 [] [
                        "Sector: " + sectorName |> str
                    ]
                ]
                charts
                [screenerResultsTable; stocksSection]
            ] |> List.concat

        let view = div [_class "content"] contentSections 
        
        [view] |> mainLayout $"Sector Dashboard for {sectorName}" 