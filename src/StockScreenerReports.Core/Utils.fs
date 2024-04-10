namespace StockScreenerReports.Core

    module Utils =
        open System

        let private nonAlphaRegex = new Text.RegularExpressions.Regex("[^a-zA-Z]")

        let cleanIndustry (industry:string) =
            nonAlphaRegex.Replace(industry, "").ToLower()
            
        let cleanCountry (country:string) =
            nonAlphaRegex.Replace(country, "").ToLower()

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
            
        let formatDateForChart (date:DateTime) =
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

        let getLatestBusinessDate forDate =
            
            let rec firstNonHoliday (date:DateTime) =
                match date.Date |> ReportsConfig.isHoliday with
                | true -> firstNonHoliday (date.AddDays(-1))
                | false -> date
            
            let refDate = forDate |> firstNonHoliday
            match refDate.DayOfWeek with
            | DayOfWeek.Saturday -> refDate.AddDays(-1).Date.Add(new TimeSpan(23,59,59))
            | DayOfWeek.Sunday -> refDate.AddDays(-2).Date.Add(new TimeSpan(23,59,59))
            | _ -> refDate

        let ageInBusinessDays (date:DateTime) =
            let latestBusiness = ReportsConfig.now() |> getLatestBusinessDate
            let dateLatestBusinessDate = date |> getLatestBusinessDate
            let diff = latestBusiness.Subtract(dateLatestBusinessDate)
            printfn "Latest business date: %A" latestBusiness
            printfn "Date latest business date: %A" dateLatestBusinessDate
            printfn "Diff: %A" diff
            diff