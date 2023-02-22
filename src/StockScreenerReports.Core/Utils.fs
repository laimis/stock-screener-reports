namespace StockScreenerReports.Core

    module Utils =
        open System

        let private nonAlphaRegex = new Text.RegularExpressions.Regex("[^a-zA-Z]")

        let cleanIndustry (industry:string) =
            nonAlphaRegex.Replace(industry, "").ToLower()

        let addDaysToClosestBusinessDay (date:DateTime) days =
            let newDate = date.AddDays(days)

            match newDate.DayOfWeek with
            | DayOfWeek.Saturday -> newDate.AddDays(2)
            | DayOfWeek.Sunday -> newDate.AddDays(1)
            | _ -> newDate

        let subtractDaysToClosestBusinessDay (date:DateTime) days =
            let newDate = date.AddDays(-days)

            match newDate.DayOfWeek with
            | DayOfWeek.Saturday -> newDate.AddDays(-1)
            | DayOfWeek.Sunday -> newDate.AddDays(-2)
            | _ -> newDate

        let listOfBusinessDates (referenceDate:DateTime) days = 
            let holidays = FinvizConfig.getTradingHolidays()

            [-days .. 0]
            |> List.map (fun i -> referenceDate.Date.AddDays(i))
            |> List.where( fun (date) ->
                date.DayOfWeek = DayOfWeek.Saturday |> not &&
                date.DayOfWeek = DayOfWeek.Sunday |> not &&
                holidays |> List.contains date.Date |> not
            )

        let convertToDateString (date:DateTime) =
            date.ToString("yyyy-MM-dd")

        let getRunDate() =
            DateTime.Now |> convertToDateString

        let getCurrentMonday() =
            let refDate = DateTimeOffset.UtcNow
            match refDate.DayOfWeek with
            | DayOfWeek.Monday -> refDate
            | DayOfWeek.Tuesday -> refDate.AddDays(-1)
            | DayOfWeek.Wednesday -> refDate.AddDays(-2)
            | DayOfWeek.Thursday -> refDate.AddDays(-3)
            | DayOfWeek.Friday -> refDate.AddDays(-4)
            | DayOfWeek.Saturday -> refDate.AddDays(-5)
            | DayOfWeek.Sunday -> refDate.AddDays(-6)
            | _ -> refDate

            
            
            