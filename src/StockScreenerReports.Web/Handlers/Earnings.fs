namespace StockScreenerReports.Web.Handlers

module Earnings =
    open StockScreenerReports.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open Giraffe.ViewEngine.Attributes
    open StockScreenerReports.Storage
    open StockScreenerReports.Core

    let handler()  =
        
        let header = div [_class "content"] [
            h1 [] [str "Earnings"]
        ]

        let startDate = System.DateTimeOffset.UtcNow.AddDays(-7)
        let endDate = System.DateTimeOffset.UtcNow.AddDays(1)

        let tickersWithEarnings = Reports.getEarningsTickers startDate endDate

        let createMapFromStocks stocks =
            stocks
            |> List.map (fun s -> s.ticker |> StockTicker.value, s)
            |> Map.ofList

        let newHighs =
            Constants.NewHighsScreenerId
            |> Reports.getStocksForScreenerAndDates startDate endDate
        let newHighsMap = newHighs |> createMapFromStocks

        let topGainers =
            Constants.TopGainerScreenerId
            |> Reports.getStocksForScreenerAndDates  startDate endDate
        let topGainersMap = topGainers |> createMapFromStocks

        let topLosers =
            Constants.TopLoserScreenerId
            |> Reports.getStocksForScreenerAndDates startDate endDate
        let topLosersMap = topLosers |> createMapFromStocks

        let newLows =
            Constants.NewLowsScreenerId
            |> Reports.getStocksForScreenerAndDates startDate endDate
        let newLowsMap = newLows |> createMapFromStocks

        let rows =
            tickersWithEarnings
            |> List.map (fun (ticker, date) ->

                let newHighs = ticker |> newHighsMap.ContainsKey |> Views.generateNewHighIcon
                let topGainers = ticker |> topGainersMap.ContainsKey |> Views.generateTopGainerIcon
                let topLosers = ticker |> topLosersMap.ContainsKey |> Views.generateTopLoserIcon
                let newLows = ticker |> newLowsMap.ContainsKey |> Views.generateNewLowIcon

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