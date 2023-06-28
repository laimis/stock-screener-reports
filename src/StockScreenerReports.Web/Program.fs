module StockScreenerReports.Web.App

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open StockScreenerReports.Web.Handlers
open StockScreenerReports.Storage
open StockScreenerReports.Web.Shared

let webApp =
    choose [
        GET >=>
            choose [
                route "/" >=> warbler (fun _ -> Dashboard.handler())
                
                route "/screeners" >=> warbler (fun _ -> ScreenerManagement.managementHandler())
                
                routef "/screeners/%i" ScreenerDashboard.handler
                routef "/screeners/%i/results/%s" ScreenerResults.handler
                route "/trends" >=> Trends.handler

                route "/search" >=> Search.handler
                route "/stocks" >=> warbler (fun _ -> StockManagement.handler())
                routef "/stocks/%s" StockDashboard.handler

                routef "/sectors/%s" SectorDashboard.handler
                routef "/industries/%s/export" IndustryDashboard.exportHandler
                routef "/industries/%s" IndustryDashboard.handler
                route "/industries" >=> warbler (fun _ -> IndustriesDashboard.handler)
                route "/cycles" >=> warbler (fun _ -> Cycles.handler)

                route "/countries" >=> warbler (fun _ -> Countries.handler())
                routef "/countries/%s" CountryDashboard.handler

                route "/reports/adhoc" >=> warbler (fun _ -> AdhocReport.handler())

                route "/earnings" >=> warbler (fun _ -> Earnings.handlerCurrentWeek())
                route "/earnings/lastweek" >=> warbler (fun _ -> Earnings.handlerLast7Days())

                route "/health" >=> HealthCheck.healthCheckHandler

                // jobs
                route Links.jobsScreeners >=> warbler (fun _ -> Jobs.screeners())
                route Links.jobsEarnings >=> warbler (fun _ -> Jobs.earnings())
                route Links.jobsTrends >=> warbler (fun _ -> Jobs.trends())

            ]
        POST >=>
            choose [
                route Links.screenersNew >=> ScreenerManagement.createHandler
                routef "/screeners/%i/delete" ScreenerManagement.deleteHandler
                
                routef "/screeners/%i/export" ScreenerManagement.exportHandler

                route "/reports/adhoc/export" >=> warbler (fun _ -> AdhocReport.exportHandler())

                route "/stocks/adjustticker" >=> StockManagement.adjustTicker

                route "/screeners/migratedate" >=> ScreenerManagement.migrateDateHandler
            ]
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder : CorsPolicyBuilder) =
    builder
        .WithOrigins(
            "http://localhost:5000",
            "https://localhost:5001")
       .AllowAnyMethod()
       .AllowAnyHeader()
       |> ignore

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    (match env.IsDevelopment() with
    | true  ->
        app.UseDeveloperExceptionPage()
    | false ->
        app .UseGiraffeErrorHandler(errorHandler)
            .UseHttpsRedirection())
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    services.AddCors()    |> ignore
    services.AddGiraffe() |> ignore

    let cnnString = System.Environment.GetEnvironmentVariable("SSR_CONNECTIONSTRING")
    cnnString |> Storage.configureConnectionString
    cnnString |> Reports.configureConnectionString

    // we also need to make sure the date is returned in easter timezone
    let easternTimeZone = TimeZoneConverter.TZConvert.GetTimeZoneInfo("Eastern Standard Time")
    StockScreenerReports.Core.TimeFunctions.nowFunc <- fun () ->
        let now = DateTime.UtcNow
        TimeZoneInfo.ConvertTimeFromUtc(now, easternTimeZone)

let configureLogging (builder : ILoggingBuilder) =
    builder.AddConsole()
           .AddDebug() |> ignore

[<EntryPoint>]
let main args =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot     = Path.Combine(contentRoot, "WebRoot")
    Host.CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseContentRoot(contentRoot)
                    .UseWebRoot(webRoot)
                    .Configure(Action<IApplicationBuilder> configureApp)
                    .ConfigureServices(configureServices)
                    .ConfigureLogging(configureLogging)
                    |> ignore)
        .Build()
        .Run()
    0