namespace StockScreenerReports.Web.Handlers

module CountryDashboard =
    open StockScreenerReports.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open StockScreenerReports.Storage
    open Giraffe.ViewEngine.Attributes
    open FinvizScraper.Core
    open StockScreenerReports.Web.Shared.Views

    let handler countryName =
        let screeners = Storage.getScreeners()

        let days = FinvizConfig.dayRange

        let list = days |> Logic.businessDatesWithZeroPairs

        let charts = 
            screeners
            |> List.map (fun screener ->
                let data = Reports.getDailyCountsForScreenerAndCountry screener.id countryName days

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
            countryName
            |> Reports.getScreenerResultsForCountry 50
            |> List.map (fun screenerResult ->
                tr [] [
                    td [] [ screenerResult.date |> Utils.convertToDateString |> str ]
                    td [] [
                        (screenerResult.screenerid,screenerResult.screenername) |> generateScreenerTags
                    ]
                    td [] [ screenerResult.ticker |> generateTickerLink ]
                    td [] [ screenerResult.marketCap |> marketCapFormatted |> str ]
                    td [] [ screenerResult.price |> dollarFormatted |> str ]
                    td [] [ screenerResult.change |> percentFormatted |> str ]
                    td [] [ screenerResult.volume |> volumeFormatted |> str ]
                    td [] [ screenerResult.ticker |> Links.tradingViewLink |> generateHref "chart" ]
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

        let screenerResultsTable = tableHeader::resultRows |> fullWidthTable

        let header = 
            div [_class "content"] [
                h1 [] [
                    str countryName
                ]
            ]

        [screenerResultsTable] |> List.append charts |> List.append [header] |> mainLayout $"Country Dashboard for {countryName}" 