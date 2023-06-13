namespace StockScreenerReports.Web.Handlers

module Jobs =
    open StockScreenerReports.Web
    open StockScreenerReports.Web.Shared.Utils
    open Giraffe
    open StockScreenerReports.Web.Shared

    let logger = new DummyLogger()
    
    let private runWithLoggerAndRedirect url func =
        func logger
        redirectTo false url

    let screeners() =
        Services.screenerRun |> runWithLoggerAndRedirect "/"

    let trends() =
        Services.trendsRun |> runWithLoggerAndRedirect Links.trends

    let earnings() =
        Services.earningsRun |> runWithLoggerAndRedirect Links.earnings