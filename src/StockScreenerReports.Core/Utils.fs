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


        let convertToDateString (date:DateTime) =
            date.ToString("yyyy-MM-dd")
        
        let convertToDateStringForOffset (date:DateTimeOffset) =
            date.ToString("yyyy-MM-dd")

        let convertToDateTime (dateString:string) =
            DateTime.ParseExact(dateString, "yyyy-MM-dd", null)

        let getRunDate() =
            ReportsConfig.now() |> convertToDateString

        let getCurrentMonday() =
            let refDate = ReportsConfig.now()
            match refDate.DayOfWeek with
            | DayOfWeek.Monday -> refDate
            | DayOfWeek.Tuesday -> refDate.AddDays(-1)
            | DayOfWeek.Wednesday -> refDate.AddDays(-2)
            | DayOfWeek.Thursday -> refDate.AddDays(-3)
            | DayOfWeek.Friday -> refDate.AddDays(-4)
            | DayOfWeek.Saturday -> refDate.AddDays(-5)
            | DayOfWeek.Sunday -> refDate.AddDays(-6)
            | _ -> refDate

            
            
            