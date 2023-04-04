namespace StockScreenerReports.Web.Shared

module Utils =
    open Charts

    let businessDatesWithZeroPairs days =
        [for i in -days .. 0 -> (System.DateTime.UtcNow.Date.AddDays(i),0) ]
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

    let genericJobStatusGet jobName =
            match (StockScreenerReports.Storage.Storage.getLatestJobStatus jobName) with
                | Some (message, timestamp) -> 
                    let age = System.DateTimeOffset.UtcNow.Subtract(timestamp)
                    let friendlyAgeString =
                        match age with
                        | age when age.TotalDays > 1.0 -> $"{(int)age.TotalDays} days ago"
                        | age when age.TotalHours > 1.0 -> $"{(int)age.TotalHours} hours ago"
                        | age when age.TotalMinutes > 1.0 -> $"{(int)age.TotalMinutes} minutes ago"
                        | _ -> "just now"
                    $"{message} @ {timestamp}, {friendlyAgeString}"
                | None ->
                    $"No results found for {jobName} found"