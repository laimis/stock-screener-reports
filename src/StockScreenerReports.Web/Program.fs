module StockScreenerReports.Web.App

open System
open System.IO
open System.Threading.Tasks
open Hangfire
open Hangfire.MemoryStorage
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Microsoft.FSharp.Linq.RuntimeHelpers
open StockScreenerReports.Core
open StockScreenerReports.Storage

// time functions return eastern timezone to match US market hours
let easternTimeZone = TimeZoneConverter.TZConvert.GetTimeZoneInfo("Eastern Standard Time")    
    
let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

let configureCors (builder : CorsPolicyBuilder) =
    builder
        .WithOrigins(
            "http://localhost:5000",
            "https://localhost:5001")
       .AllowAnyMethod()
       .AllowAnyHeader()
       |> ignore
  
let configureJobs (app : IApplicationBuilder) =
    let skipBackgroundJobs = Environment.GetEnvironmentVariable("SSR_SKIPBACKGROUNDJOBS")
    let logger = app.ApplicationServices.GetService<ILogger<Services>>()
    match skipBackgroundJobs with
    | "true" ->
        logger.LogInformation("Skipping background jobs")
    | _  ->
        logger.LogInformation("Configuring background jobs")
        
        let rjo = RecurringJobOptions()
        rjo.TimeZone <- easternTimeZone
        
        RecurringJob.AddOrUpdate<Services>(
            recurringJobId="fullrun",
            methodCall=(fun (s:Services) -> s.FullRun() :> Task),
            cronExpression=Cron.Daily(17, 00),
            options=rjo
        )
        
        logger.LogInformation("Starting background jobs")

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    (match env.IsDevelopment() with
    | true  ->
        app.UseDeveloperExceptionPage()
    | false ->
        app.UseGiraffeErrorHandler(errorHandler)
            .UseHttpsRedirection())
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseAuthentication()
        .UseAuthorization()
        .UseGiraffe(Router.routes)
        
    configureJobs app 

let configureServices (services : IServiceCollection) =
    services.AddCors()    |> ignore
    services.AddGiraffe() |> ignore
    
    services.AddAuthentication(CookieAuthenticationDefaults.AuthenticationScheme)
        .AddCookie() |> ignore
    services.AddAuthorization() |> ignore
    
    let cnnString = Environment.GetEnvironmentVariable("SSR_CONNECTIONSTRING")
    cnnString |> Storage.configureConnectionString
    cnnString |> Reports.configureConnectionString
    
    services.AddSingleton<Services>() |> ignore
    services.AddHangfire(fun c -> c.UseMemoryStorage() |> ignore) |> ignore
    services.AddHangfireServer() |> ignore

    TimeFunctions.nowFunc <- fun () ->
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
                    .ConfigureLogging(configureLogging)
                    .Configure(Action<IApplicationBuilder> configureApp)
                    .ConfigureServices(configureServices)
                    |> ignore)
        .Build()
        .Run()
    0