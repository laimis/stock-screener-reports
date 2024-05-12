namespace StockScreenerReports.Web.Handlers

open StockScreenerReports.Core
open StockScreenerReports.Web.Shared.Utils

module Jobs =
    open StockScreenerReports.Web
    open StockScreenerReports.Web.Shared.Utils
    open Giraffe
    open StockScreenerReports.Web.Shared

    let logger = DummyLogger()
    
    let private runWithLoggerAndRedirect url (func:DummyLogger -> System.Threading.Tasks.Task<JobStatus>) : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) -> task {
            let! _ = func logger // TODO: don't redirect if failed, show the failure
            return! redirectTo false url next ctx
        }

    let screeners() =
        Services.screenerRun |> runWithLoggerAndRedirect "/"

    let trends() =
        Services.trendsRun |> runWithLoggerAndRedirect Links.trends
        
    let countries() =
        Services.countriesRun |> runWithLoggerAndRedirect Links.countries

    let earnings() =
        Services.earningsRun |> runWithLoggerAndRedirect Links.earnings
        
    let alerts() =
        Services.alertsRun |> runWithLoggerAndRedirect Links.alerts
        
    let corporateActions() =
        Services.corporateActionsRun true |> runWithLoggerAndRedirect Links.corporateActions