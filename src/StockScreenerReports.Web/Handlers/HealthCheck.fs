namespace StockScreenerReports.Web.Handlers
open Giraffe
open Microsoft.AspNetCore.Http

module HealthCheck =
    open Microsoft.Extensions.Diagnostics.HealthChecks
    
    let healthCheckHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                return! json (HealthCheckResult(HealthStatus.Healthy)) next ctx
            }