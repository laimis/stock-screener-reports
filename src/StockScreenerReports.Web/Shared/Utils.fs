namespace StockScreenerReports.Web.Shared

module Utils =
    open Charts
    open StockScreenerReports.Core
    open Microsoft.Extensions.Logging

    let businessDatesWithZeroPairs days =
        [for i in -days .. 0 -> (ReportsConfig.now().Date.AddDays(i),0) ]
        |> List.where (fun (date,_) ->
            date.DayOfWeek = System.DayOfWeek.Saturday |> not && date.DayOfWeek = System.DayOfWeek.Sunday |> not
        )

    let smoothedDataSets windowSize (datasets:DataSet<decimal> list) =
        datasets
        |> List.map (fun d -> 
            
            let windowed =
                d.data
                |> List.windowed windowSize
                |> List.map (fun u -> u |> List.average |> System.Math.Round)

            { d with data = windowed}
        )
    
    let failedJobFilter (job:Job) =
        match job.status with
        | Failure -> true
        | _ -> 
            job.timestamp |> Utils.ageInBusinessDays > System.TimeSpan.FromDays(1.5)

    type DummyLogger() =
        interface ILogger with
            member this.BeginScope(state: 'TState): System.IDisposable = 
                failwith "Not Implemented"
            member this.IsEnabled(logLevel: LogLevel): bool = 
                true
            member this.Log(logLevel: LogLevel, eventId: EventId, state: 'TState, ``exception``: exn, formatter: System.Func<'TState,exn,string>): unit = 
                let message = formatter.Invoke(state, ``exception``)

                match logLevel with
                | LogLevel.Trace -> printfn $"Trace: {message}"
                | LogLevel.Debug -> printfn $"Debug: {message}"
                | LogLevel.Information -> printfn $"Information: {message}"
                | LogLevel.Warning -> printfn $"Warning: {message}"
                | LogLevel.Error -> printfn $"Error: {message}"
                | LogLevel.Critical -> printfn $"Critical: {message}"
                | LogLevel.None -> printfn $"None: {message}"
                | _ -> printfn $"Unknown: {message}"