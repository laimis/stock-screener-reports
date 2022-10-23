namespace StockScreenerReports.Web.Handlers

module Earnings =
    open StockScreenerReports.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open Giraffe.ViewEngine.Attributes
    open StockScreenerReports.Storage
    open FSharp.Data
    open Giraffe

    let handler()  =
        
        let header = div [_class "content"] [
            h1 [] [str "Earnings"]
        ]

        let startDate = System.DateTimeOffset.UtcNow.AddDays(-7)
        let endDate = System.DateTimeOffset.UtcNow.AddDays(1)

        let tickersWithEarnings = Reports.getEarningsTickers startDate endDate

        let newHighs =
            Reports.getTickersForScreenerAndDates StockScreenerReports.Core.Constants.NewHighsScreenerId startDate endDate

        let topGainers =
            Reports.getTickersForScreenerAndDates StockScreenerReports.Core.Constants.TopGainerScreenerId startDate endDate

        let topLosers =
            Reports.getTickersForScreenerAndDates StockScreenerReports.Core.Constants.TopLoserScreenerId startDate endDate

        let newLows =
            Reports.getTickersForScreenerAndDates StockScreenerReports.Core.Constants.NewLowsScreenerId startDate endDate

        let rows =
            tickersWithEarnings
            |> List.map (fun (ticker, date) ->

                let newHighs = newHighs |> List.contains ticker |> Views.generateNewHighIcon
                let topGainers = topGainers |> List.contains ticker |> Views.generateTopGainerIcon
                let topLosers = topLosers |> List.contains ticker |> Views.generateTopLoserIcon
                let newLows = newLows |> List.contains ticker |> Views.generateNewLowIcon

                tr [] [
                    td [] [ticker |> Views.generateTickerLink]
                    td [] [date.ToString("yyyy-MM-dd") |> str]
                    td [] [newHighs]
                    td [] [topGainers]
                    td [] [topLosers]
                    td [] [newLows]
                    td [] [ticker |> Links.tradingViewLink |> Views.generateHrefNewTab "link"]
                ]
            )

        let earningsTable = 
            table [_class "table is-striped is-fullwidth"] [
                thead [] [
                    tr [] [
                        th [] [str "Ticker"]
                        th [] [str "Date"]
                        th [] [str "New High"]
                        th [] [str "Top Gainer"]
                        th [] [str "Top Loser"]
                        th [] [str "New Low"]
                        th [] [str "Trading View"]
                    ]
                ]
                tbody [] rows
            ]
        
        [header; earningsTable] |> Views.mainLayout $"Earnings"