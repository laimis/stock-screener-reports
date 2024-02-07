namespace StockScreenerReports.Web

open Giraffe
open StockScreenerReports.Web.Handlers
open StockScreenerReports.Web.Shared

module Router =
    let routes : HttpHandler =
        choose [
            GET >=>
                choose [
                    route "/" >=> warbler (fun _ -> Dashboard.handler())
                    
                    route Links.screeners >=> warbler (fun _ -> ScreenerManagement.managementHandler())
                    
                    routef "/screeners/%i" ScreenerDashboard.handler
                    routef "/screeners/%i/results/export" ScreenerResults.exportHandler
                    routef "/screeners/%i/results/%s" ScreenerResults.handler
                    route Links.trends >=> Trends.handler

                    route Links.searchLink >=> Search.handler
                    route "/stocks" >=> warbler (fun _ -> StockManagement.handler())
                    routef "/stocks/%s" StockDashboard.handler

                    routef "/sectors/%s" SectorDashboard.handler
                    routef "/industries/%s/export" IndustryDashboard.exportHandler
                    routef "/industries/%s" IndustryDashboard.handler
                    route Links.industries >=> warbler (fun _ -> IndustriesDashboard.handler)
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
                    
                    // test endpoint
                    route "/screeners/diagnostics" >=> warbler (fun _ -> ScreenerManagement.checkScannerHandler())

                ]
            POST >=>
                choose [
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
                ]
            setStatusCode 404 >=> text "Not Found" ]