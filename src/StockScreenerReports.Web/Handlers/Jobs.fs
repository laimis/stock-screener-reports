namespace StockScreenerReports.Web.Handlers

open Giraffe
open StockScreenerReports.Core
open StockScreenerReports.Web
open StockScreenerReports.Web.Shared.Utils
open StockScreenerReports.Web.Shared


module Jobs =
    
    let logger = DummyLogger()
    
    let private runWithLoggerAndRedirect url (func:Services -> System.Threading.Tasks.Task<JobStatus>) : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) -> task {
            let services = ctx.GetService<Services>()
            let! _ = func services
            // TODO: don't redirect if failed, show the failure
            return! redirectTo false url next ctx
        }

    let screeners() =
        (fun (s:Services) -> s.ScreenerRun()) |> runWithLoggerAndRedirect "/"

    let trends() =
        (fun (s:Services) -> s.TrendsRun()) |> runWithLoggerAndRedirect Links.trends
        
    let countries() =
        (fun (s:Services) -> s.CountriesRun()) |> runWithLoggerAndRedirect Links.countries

    let earnings() =
        (fun (s:Services) -> s.EarningsRun()) |> runWithLoggerAndRedirect Links.earnings
        
    let alerts() =
        (fun (s:Services) -> s.AlertsRun()) |> runWithLoggerAndRedirect Links.alerts
        
    let corporateActions() =
        (fun (s:Services) -> s.CorporateActionsRun true) |> runWithLoggerAndRedirect Links.corporateActions