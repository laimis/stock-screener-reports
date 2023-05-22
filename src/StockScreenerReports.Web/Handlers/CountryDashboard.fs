namespace StockScreenerReports.Web.Handlers

module CountryDashboard =
    open StockScreenerReports.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open StockScreenerReports.Storage
    open Giraffe.ViewEngine.Attributes
    open StockScreenerReports.Core
    open StockScreenerReports.Web.Shared.Views

    let handler countryName =
        let screeners = Storage.getScreeners()

        let dateRange = ReportsConfig.dateRange
        let dateRangeAsStrings = ReportsConfig.dateRangeAsStrings

        let list = dateRange |> ReportsConfig.listOfBusinessDates

        let charts = 
            screeners
            |> List.map (fun screener ->
                let data = Reports.getDailyCountsForScreenerAndCountry screener.id countryName dateRangeAsStrings

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
            countryName
            |> Reports.getScreenerResultsForCountry 50
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

        let tableHeader = [
            "Date"; "Screener"; "Ticker"; "Market Cap"; "Price"; "Change"; "Volume"; "Trading View"
        ]
        let screenerResultsTable = resultRows |> fullWidthTable tableHeader

        let header = 
            div [_class "content"] [
                h1 [] [
                    str countryName
                ]
            ]

        [screenerResultsTable] |> List.append charts |> List.append [header] |> mainLayout $"Country Dashboard for {countryName}" 