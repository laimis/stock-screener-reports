namespace StockScreenerReports.Web.Handlers

open XPlot.Plotly

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
            Durations = durations
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

    let private view (industryBreakdowns: IndustrySMABreakdown list list) =
        let stats =
            industryBreakdowns
            |> List.map (fun breakdownList ->
                    breakdownList
                    |> List.map (fun r -> r.breakdown.percentAbove)
                    |> analyzeSequence
                    |> describeDurations, breakdownList.Head.industry
            )
            |> List.filter (fun (l,_) -> l.Count > 2)
            
        let combined = stats |> List.collect (fun (s,_) -> s.Durations) |> describeDurations
        let combinedStats = combined, "All Industries"
        
            
        let industryBoxes =
            [combinedStats] @ stats
            |> List.map(fun (stats, industry) ->
                let columnData = stats.Durations |> List.sort |> List.mapi (fun i d -> (i, d))
                
                let columnChart =
                    Chart.Column(columnData)
                    |> Chart.WithSize (400, 300)
                
                let layout = Layout(bargap= 0.1, bargroupgap = 0.1)
                let histogram =
                    Histogram(x = stats.Durations, nbinsx = 10)
                    |> Chart.Plot
                    |> Chart.WithLayout layout
                    |> Chart.WithSize (400, 300)
                
                
                div [_class "box"] [
                    div [_class "media"] [
                        div [_class "media-content"] [
                            p [_class "title is-4"] [str industry]
                        ]
                    ]
                    div [_class "content"] [
                        table [_class "table is-bordered is-striped is-narrow is-hoverable"] [
                            thead [] [
                                tr [] [
                                    th [] [str "Metric"]
                                    th [] [str "Value"]
                                ]
                            ]
                            tbody [] [
                                tr [] [
                                    
                                    td [] [str "Durations"]
                                    td [] [str (stats.Count.ToString())]
                                ]
                                tr [] [
                                    td [] [str "Min"]
                                    td [] [str (stats.MinDuration.ToString())]
                                ]
                                tr [] [
                                    td [] [str "Max"]
                                    td [] [str (stats.MaxDuration.ToString())]
                                ]
                                tr [] [
                                    td [] [str "Mean"]
                                    td [] [str (stats.AverageDuration.ToString("F2"))]
                                ]
                                tr [] [
                                    td [] [str "Median"]
                                    td [] [str (stats.MedianDuration.ToString())]
                                ]
                                tr [] [
                                    td [] [columnChart.GetInlineHtml() |> rawText]
                                    td [] [histogram.GetInlineHtml() |> rawText]
                                ]
                            ]
                        ]
                    ]
                ])

        div [_class "container"] [
            h1 [_class "title"] [str "Industry Trend Sequences"]
            div [_class "box-container"] industryBoxes
        ]

    let handler () =
        
        Storage.getIndustries()
        |> List.map (Reports.getAllIndustrySMABreakdowns SMA20)
        |> view
        |> List.singleton
        |> mainLayout "Industry SMA Breakdown Trends"
