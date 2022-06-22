namespace FinvizScraper.Web.Shared

module Logic =

    let businessDatesWithZeroPairs days =
        [for i in -days .. 0 -> (System.DateTime.UtcNow.Date.AddDays(i),0) ]
        |> List.where (fun (date,_) ->
            date.DayOfWeek = System.DayOfWeek.Saturday |> not && date.DayOfWeek = System.DayOfWeek.Sunday |> not
        )