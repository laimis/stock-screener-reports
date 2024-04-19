module StockScreenerReports.Web.Handlers.Alerts
    
    
    open Giraffe
    open Microsoft.AspNetCore.Http
    open StockScreenerReports.Storage
    
    let acknowledgeAlertHandler alertIdentifier : HttpHandler =
        fun (_: HttpFunc) (ctx: HttpContext) ->
            task {
                
                let alerts = Storage.getAlerts()
                
                let alert = alerts |> List.tryFind (fun a -> a.identifier = alertIdentifier)
                
                match alert with
                | Some a ->
                    let acknowledgedAlert = { a with acknowledged = true }
                    Storage.saveAlert acknowledgedAlert |> ignore
                | None ->
                    ctx.Response.StatusCode <- 404
                
                return! ctx.WriteJsonAsync()
            }