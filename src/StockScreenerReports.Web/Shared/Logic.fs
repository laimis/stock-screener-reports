namespace StockScreenerReports.Web.Shared

module Logic =
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