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

        let days = FinvizConfig.dayRange

        let list = days |> Logic.businessDatesWithZeroPairs

        let charts = 
            screeners
            |> List.map (fun screener ->
                let data = Reports.getDailyCountsForScreenerAndSector screener.id sectorName days

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
            sectorName
            |> Reports.getScreenerResultsForSector 50
            |> List.map (fun screenerResult ->
                tr [] [
                    td [] [ screenerResult.date |> Utils.convertToDateString |> str ]
                    td [] [
                        (screenerResult.screenerid,screenerResult.screenername) |> Views.generateScreenerTags
                    ]
                    td [] [ screenerResult.ticker |> generateTickerLink ]
                    td [] [ screenerResult.marketCap |> marketCapFormatted |> str ]
                    td [] [ screenerResult.price |> dollarFormatted |> str ]
                    td [] [ screenerResult.change |> percentFormatted |> str ]
                    td [] [ screenerResult.volume |> volumeFormatted |> str ]
                    td [] [ screenerResult.ticker |> Links.tradingViewLink |> generateHref "chart" ]
                ]
            )

        let headerCells = [
            "date"
            "screener"
            "ticker"
            "market cap"
            "price"
            "change"
            "volume"
            "trading view"
        ]

        let screenerResultsTable = resultRows |> fullWidthTable headerCells

        let stocks = Storage.getStocksBySector sectorName

        let stockTableHeaders = [
            "ticker"
            "company"
            "sector"
            "industry"
        ]

        let stockTable =
            stocks
            |> List.map (fun stock ->
                tr [] [
                    td [] [
                        stock.ticker |> StockTicker.value |> Views.generateTickerLink
                    ]
                    td [] [str stock.company]
                    td [] [ generateHref stock.sector (Links.sectorLink stock.sector) ]
                    td [] [ generateHref stock.industry (Links.industryLink stock.industry) ]
                ]
            )
            |> fullWidthTable stockTableHeaders

        let view = 
            div [_class "content"] [
                h1 [] [
                    "Sector: " + sectorName |> str
                ]
            ]::charts @ [screenerResultsTable; stockTable]
            
        
        view |> mainLayout $"Sector Dashboard for {sectorName}" 