module StockScreenerReports.Web.App

open System
open System.IO
open Microsoft.AspNetCore.Authentication.Cookies
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open StockScreenerReports.Storage

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
        .UseAuthentication()
        .UseAuthorization()
        .UseGiraffe(Router.routes)

let configureServices (services : IServiceCollection) =
    services.AddCors()    |> ignore
    services.AddGiraffe() |> ignore
    
    services.AddAuthentication(CookieAuthenticationDefaults.AuthenticationScheme)
        .AddCookie() |> ignore
    services.AddAuthorization() |> ignore

    let cnnString = Environment.GetEnvironmentVariable("SSR_CONNECTIONSTRING")
    cnnString |> Storage.configureConnectionString
    cnnString |> Reports.configureConnectionString

    // we also need to make sure the date is returned in easter timezone
    let easternTimeZone = TimeZoneConverter.TZConvert.GetTimeZoneInfo("Eastern Standard Time")
    StockScreenerReports.Core.TimeFunctions.nowFunc <- fun () ->
        let now = DateTime.UtcNow
        TimeZoneInfo.ConvertTimeFromUtc(now, easternTimeZone)

    let skipBackgroundJobs = Environment.GetEnvironmentVariable("SSR_SKIPBACKGROUNDJOBS")
    match skipBackgroundJobs with
    | "true" -> System.Console.WriteLine("Skipping background jobs") |> ignore
    | _  -> services.AddHostedService<Services.BackgroundService>() |> ignore 

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