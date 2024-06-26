namespace StockScreenerReports.Web

open Giraffe
open Microsoft.AspNetCore.Http
open StockScreenerReports.Core
open StockScreenerReports.Web.Handlers
open StockScreenerReports.Web.Shared

module Router =
    let requiresAuthentication : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            if ctx.User.Identity.IsAuthenticated then
                next ctx
            else
                // capture the path that was requested
                let path = ctx.Request.Path.ToString() |> System.Web.HttpUtility.UrlEncode
                
                ctx.SetStatusCode 401
                redirectTo false $"/login?returnUrl={path}" next ctx
            
    let routes : HttpHandler =
        choose [
            
            
            GET >=>
                choose [
                    route "/login" >=> LoginHandler.loginHandler
                    route "/logout" >=> LoginHandler.logoutHandler
                                
                    // allow exporting of data without auth to allow 3rd party connections without auth
                    route Links.exportSMA20 >=> warbler (fun _ -> Dashboard.exportSma20Handler())
                    route Links.exportCycleStarts >=> warbler (fun _ -> Dashboard.exportCycleStartsHandler())
                    route Links.exportCycleHighs >=> warbler (fun _ -> Dashboard.exportCycleHighsHandler())
                    routef "/screeners/%i/results/export" ScreenerResults.exportHandler
                                            
                    requiresAuthentication >=>
                        route Links.dashboard >=> Dashboard.handler
                        route Links.screeners >=> warbler (fun _ -> ScreenerManagement.managementHandler())
                        routef "/screeners/%i" ScreenerDashboard.handler
                        routef "/screeners/%i/results/%s" ScreenerResults.handler
                        route Links.trends >=> Dashboard.handler

                        route Links.searchLink >=> Search.handler
                        route "/stocks" >=> warbler (fun _ -> StockManagement.handler())
                        routef "/stocks/%s" StockDashboard.handler

                        routef "/sectors/%s" SectorDashboard.handler
                        routef "/industries/%s/export" IndustryDashboard.exportHandler
                        routef "/industries/%s" IndustryDashboard.handler
                        route Links.industries >=> warbler (fun _ -> IndustriesDashboard.handler)
                        route Links.industriestable >=> warbler (fun _ -> IndustriesTable.handler)
                        route Links.industryCorrelations >=> warbler (fun _ -> IndustryCorrelations.handler)
                        route Links.industrySequenceAnalysis >=> warbler (fun _ -> IndustrySequenceAnalysis.handler)
                        route Links.cycles >=> warbler (fun _ -> Cycles.handler)

                        route Links.countries >=> warbler (fun _ -> Countries.handler)
                        routef "/countries/%s" CountryDashboard.handler

                        route "/reports/adhoc" >=> warbler (fun _ -> AdhocReport.handler())

                        route Links.earnings >=> warbler (fun _ -> Earnings.handlerCurrentWeek())
                        route "/earnings/lastweek" >=> warbler (fun _ -> Earnings.handlerLast7Days())

                        route "/analysis" >=> Analysis.handler

                        route "/health" >=> HealthCheck.healthCheckHandler

                        // jobs
                        route Links.jobsScreeners >=> warbler (fun _ -> Jobs.screeners())
                        route Links.jobsEarnings >=> warbler (fun _ -> Jobs.earnings())
                        route Links.jobsTrends >=> warbler (fun _ -> Jobs.trends())
                        route Links.jobsCountries >=> warbler (fun _ -> Jobs.countries())
                        route Links.jobsAlerts >=> warbler (fun _ -> Jobs.alerts())
                        route Links.jobsCorporateActions >=> warbler (fun _ -> Jobs.corporateActions())
                        
                        // test endpoint
                        route "/screeners/diagnostics" >=> warbler (fun _ -> ScreenerManagement.checkScannerHandler())
                        
                        route Links.corporateActions >=> CorporateActions.handler
                        route Links.corporateActionsBankruptcy  >=> CorporateActions.bankruptcyProcessing
                        route Links.corporateActionsDelisting  >=> CorporateActions.delistingProcessing
                ]
            POST >=>
                choose [
                    route "/login" >=> LoginHandler.verifyLoginHandler
                    
                    requiresAuthentication >=>
                        route Links.screenersNew >=> ScreenerManagement.createHandler
                        routef "/screeners/%i/delete" ScreenerManagement.deleteHandler
                        routef "/screeners/%i" ScreenerDashboard.createHandler
                        route "/reports/adhoc/export" >=> warbler (fun _ -> AdhocReport.exportHandler())

                        route "/stocks/adjustticker" >=> StockManagement.adjustTicker

                        route Links.migrateDateLink >=> ScreenerManagement.migrateDateHandler
                        route Links.deleteDateLink >=> ScreenerManagement.deleteDateHandler
                        route Links.renameStockLink >=> ScreenerManagement.renameStockHandler
                        route Links.deleteStockLink >=> ScreenerManagement.deleteStockHandler
                        route Links.changeStockIndustryLink >=> ScreenerManagement.changeStockIndustryHandler

                        route "/analysis/tickers" >=> Analysis.analyzeHandler
                        
                        routef "/alerts/acknowledge/%s" Alerts.acknowledgeAlertHandler
                ]
            setStatusCode 404 >=> text "Not Found" ]