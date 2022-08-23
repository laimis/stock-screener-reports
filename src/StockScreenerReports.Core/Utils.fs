namespace FinvizScraper.Core

    module Utils =
        open System

        let private nonAlphaRegex = new System.Text.RegularExpressions.Regex("[^a-zA-Z]")

        let cleanIndustry (industry:string) =
            nonAlphaRegex.Replace(industry, "").ToLower()

        let addDaysToClosestBusinessDay (date:System.DateTime) days =
            let newDate = date.AddDays(days)

            match newDate.DayOfWeek with
            | DayOfWeek.Saturday -> newDate.AddDays(2)
            | DayOfWeek.Sunday -> newDate.AddDays(1)
            | _ -> newDate

        let subtractDaysToClosestBusinessDay (date:System.DateTime) days =
            let newDate = date.AddDays(-days)

            match newDate.DayOfWeek with
            | DayOfWeek.Saturday -> newDate.AddDays(-1)
            | DayOfWeek.Sunday -> newDate.AddDays(-2)
            | _ -> newDate

        let convertToDateString (date:DateTime) =
            date.ToString("yyyy-MM-dd")

        let getRunDate() =
            DateTime.Now |> convertToDateString 