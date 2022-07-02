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
