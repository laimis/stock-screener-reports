namespace StockScreenerReports.Web.Handlers

open StockScreenerReports.Web.Shared

module IndustryTrendsSummary =

    open Giraffe.ViewEngine
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared.Views
    open StockScreenerReports.Core

    let private describeDurations (durations:int list) =
        let count = durations.Length

        let averageDuration = if count > 0 then (durations |> List.sum |> decimal) / (decimal count) else 0m
        let medianDuration = if count > 0 then durations |> List.sort |> (fun l -> l[l.Length / 2]) else 0
        let maxDuration = if count > 0 then durations |> List.max else 0
        let minDuration = if count > 0 then durations |> List.min else 0

        {|
            Count = count
            AverageDuration = averageDuration
            MedianDuration = medianDuration
            MaxDuration = maxDuration
            MinDuration = minDuration
        |}

    let private analyzeSequence (values: decimal list) =
        let folder (prevDuration, durations) current =
            if current >= 90m then
                let newDuration = prevDuration + 1
                (newDuration, durations)
            else
                let updatedDurations = if prevDuration > 0 then prevDuration :: durations else durations
                (0, updatedDurations)

        let initialState = (0, [])
        let lastDuration, durations = List.fold folder initialState values

        if lastDuration > 0 then lastDuration :: durations else durations

    let private view (industries: string list) =
        let industryRows =
            industries
            |> List.sort
            |> List.map (fun industry ->
                    industry
                    |> Reports.getAllIndustrySMABreakdowns SMA.SMA20
                    |> List.map (fun r -> r.breakdown.percentAbove)
                    |> analyzeSequence
                    |> describeDurations, industry
            )
            |> List.filter (fun (l,_) -> l.Count > 2)
            |> List.map(fun (stats, industry) ->
                tr [] [
                    LinkColumn(industry, industry |> Links.industryLink) |> toTd
                    td [] [str (stats.Count.ToString())]
                    td [] [str (stats.AverageDuration.ToString("F2"))]
                    td [] [str (stats.MedianDuration.ToString())]
                    td [] [str (stats.MaxDuration.ToString())]
                    td [] [str (stats.MinDuration.ToString())]
                ])

        let headers = ["Industry"; "Occurrences"; "Avg Duration"; "Median Duration"; "Max Duration"; "Min Duration"]

        fullWidthTableWithSortableHeaderCells headers industryRows
        

    let handler () =
        let industries = Storage.getIndustries()
        let view = view industries
        [view] |> mainLayout "Industry SMA Breakdown Trends"
