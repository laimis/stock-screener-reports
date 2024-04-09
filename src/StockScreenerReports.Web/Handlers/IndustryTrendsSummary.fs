namespace StockScreenerReports.Web.Handlers

open StockScreenerReports.Web.Shared
open XPlot.Plotly

module IndustryTrendsSummary =

    open Giraffe.ViewEngine
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared.Views
    open StockScreenerReports.Core

    let private describeDurations (sequences:SMABreakdown list list) =
        let count = sequences.Length
        
        let lengths = sequences |> List.map _.Length

        let averageDuration = if count > 0 then (lengths |> List.sum |> decimal) / (decimal count) else 0m
        let medianDuration = if count > 0 then lengths |> List.sort |> (fun l -> l[l.Length / 2]) else 0
        let maxDuration = if count > 0 then lengths |> List.max else 0
        let minDuration = if count > 0 then lengths |> List.min else 0

        {|
            Count = count
            AverageDuration = averageDuration
            MedianDuration = medianDuration
            MaxDuration = maxDuration
            MinDuration = minDuration
            Sequences = sequences
            Lengths = lengths
        |}

    let private analyzeSequence (values: SMABreakdown list) =
        let folder (prevSequence, sequences) (current:SMABreakdown) =
            if current.percentAbove >= 90m then
                let updatedSequence = current :: prevSequence
                (updatedSequence, sequences)
            else
                let updatedSequences = if prevSequence <> [] then prevSequence :: sequences else sequences
                ([], updatedSequences)

        let initialState = ([], [])
        let lastSequence, sequences = List.fold folder initialState values

        if lastSequence <> [] then lastSequence :: sequences else sequences

    let private view (industryBreakdowns: IndustrySMABreakdown list list) =
        let stats =
            industryBreakdowns
            |> List.map (fun breakdownList ->
                    breakdownList
                    |> List.map (_.breakdown)
                    |> analyzeSequence
                    |> describeDurations, breakdownList.Head.industry
            )
            |> List.filter (fun (l,_) -> l.Count > 2)
            
        let combined = stats |> List.collect (fun (s,_) -> s.Sequences) |> describeDurations
        let combinedStats = combined, "All Industries"
            
        let industryBoxes =
            [combinedStats] @ stats
            |> List.map(fun (stats, industry) ->
                let columnData = stats.Lengths |> List.sort |> List.mapi (fun i d -> (i, d))
                
                let columnChart =
                    Chart.Column(columnData)
                    |> Chart.WithSize (400, 300)
                
                let layout = Layout(bargap= 0.1, bargroupgap = 0.1)
                let histogram =
                    Histogram(x = stats.Lengths, nbinsx = 10)
                    |> Chart.Plot
                    |> Chart.WithLayout layout
                    |> Chart.WithSize (400, 300)
                
                div [_class "box"] [
                    div [_class "media"] [
                        div [_class "media-content"] [
                            p [_class "title is-4"] [industry |> Links.industryLink |> generateHref industry]
                        ]
                    ]
                    div [_class "content"] [
                        table [_class "table is-bordered is-striped is-narrow is-hoverable"] [
                            thead [] [
                                tr [] (["Metric"; "Value"] |> List.map toHeaderCell)
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
                        
                        // list all sequences by date, but only if the industry is not "All Industries"
                        if industry <> "All Industries" then
                            table [_class "table is-bordered is-striped is-narrow is-hoverable"] [
                                thead [] [
                                    tr [] (["Start"; "End"; ""] |> List.map toHeaderCell)
                                ]
                                tbody []
                                    (
                                        stats.Sequences
                                        |> List.map (fun seq ->
                                            tr [] [
                                                let start = seq[seq.Length-1].date
                                                let end' = seq.Head.date
                                                let link = industry |> Links.industryLinkWithStartAndEndDate (start.AddDays(-30)) (end'.AddDays(30))
                                                
                                                td [] [link |> generateHrefNewTab (start.ToString("yyyy-MM-dd"))]
                                                td [] [link |> generateHrefNewTab (end'.ToString("yyyy-MM-dd"))]
                                                td [] [
                                                    div [_style $"background-color: #1f77b4; height: 20px; width: {seq.Length * 20}px"] []
                                                ]
                                            ]
                                        )
                                    )
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
