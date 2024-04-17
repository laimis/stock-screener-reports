namespace StockScreenerReports.Web.Handlers

open StockScreenerReports.Core
open StockScreenerReports.Web.Shared
open XPlot.Plotly

module IndustrySequenceAnalysis =

    open Giraffe
    open Giraffe.ViewEngine
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared.Views

    let private describeDurations selectedType (allsequences:IndustrySequence list) =
        let sequences = allsequences |> List.filter (fun s -> s.type' = selectedType)
        let count = sequences.Length
        
        let lengths = sequences |> List.map _.values.Length

        let averageDuration = if count > 0 then (lengths |> List.sum |> decimal) / (decimal count) else 0m
        let medianDuration = if count > 0 then lengths |> List.sort |> (fun l -> l[l.Length / 2]) else 0
        let maxDuration = if count > 0 then lengths |> List.max else 0
        let minDuration = if count > 0 then lengths |> List.min else 0

        {|
            Count = count
            AverageDuration = System.Math.Round(averageDuration, 2)
            MedianDuration = medianDuration
            MaxDuration = maxDuration
            MinDuration = minDuration
            Sequences = sequences
            Lengths = lengths
        |}

    let private view selectedSequenceType (industryBreakdowns: IndustrySMABreakdown list list) =
        let stats =
            industryBreakdowns
            |> List.map (fun breakdownList ->
                    breakdownList
                    |> TrendsCalculator.calculateSequences
                    |> describeDurations selectedSequenceType, breakdownList.Head.industry
            )
            |> List.filter (fun (l,_) -> l.Count > 2)
            
        let combined = stats |> List.collect (fun (s,_) -> s.Sequences) |> describeDurations selectedSequenceType
        let combinedStats = combined, "All Industries"
            
        let industryBoxes =
            [combinedStats] @ stats
            |> List.map(fun (industryStats, industry) ->
                let columnData = industryStats.Lengths |> List.sort |> List.mapi (fun i d -> (i, d))
                
                let columnChart =
                    Chart.Column(columnData)
                    |> Chart.WithSize (400, 300)
                
                let layout = Layout(bargap= 0.1, bargroupgap = 0.1)
                let histogram =
                    Histogram(x = industryStats.Lengths, nbinsx = 10)
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
                                let toRow name value = tr [] [td [] [str name]; td [] [value.ToString() |> str]]
                                 
                                toRow "Durations" industryStats.Count
                                toRow "Min" industryStats.MinDuration
                                toRow "Max" industryStats.MaxDuration
                                toRow "Mean" industryStats.AverageDuration
                                toRow "Median" industryStats.MedianDuration
                                
                                tr [] [
                                    columnChart.GetInlineHtml() |> rawText |> toTdWithNode
                                    histogram.GetInlineHtml() |> rawText |> toTdWithNode
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
                                        industryStats.Sequences
                                        |> List.map (fun seq ->
                                            tr [] [
                                                let start = seq.start.date
                                                let end' = seq.end'.date
                                                let link = industry |> Links.industryLinkWithStartAndEndDate (start.AddDays(-30)) (end'.AddDays(30))
                                                
                                                td [] [link |> generateHrefNewTab (start.ToString("yyyy-MM-dd"))]
                                                td [] [link |> generateHrefNewTab (end'.ToString("yyyy-MM-dd"))]
                                                td [] (sequenceToDurationBarChart seq)
                                            ]
                                        )
                                    )
                            ]
                        else
                            // for all case, get the 10 longest sequences and show industry and start date
                            table [_class "table is-bordered is-striped is-narrow is-hoverable"] [
                                thead [] [
                                    tr [] (["Industry"; "Start"; ""] |> List.map toHeaderCell)
                                ]
                                tbody []
                                    (
                                        stats
                                        |> List.map  (fun (s,industry) ->
                                            s.Sequences |> List.map (fun seq -> seq, industry)
                                        )
                                        |> List.concat
                                        |> List.sortByDescending (fun (s,_) -> s.values.Length)
                                        |> List.take 20
                                        |> List.map (fun (seq,industry) ->
                                            tr [] [
                                                let start = seq.start.date
                                                let end' = seq.end'.date
                                                let link = industry |> Links.industryLinkWithStartAndEndDate (start.AddDays(-30)) (end'.AddDays(30))
                                                
                                                td [] [link |> generateHrefNewTab industry]
                                                td [] [link |> generateHrefNewTab (start.ToString("yyyy-MM-dd"))]
                                                td [] (sequenceToDurationBarChart seq)
                                            ]
                                        )
                                    )
                            ]
                    ]
                ])

        let sequenceTypes = [High; Low]
        let onChangeAttribute = XmlAttribute.KeyValue("onchange", "location = this.value;")
        
        div [_class "container"] [
            h1 [_class "title"] [str "Industry Trend Sequences"]
            div [ _class "select" ] [
                select [ onChangeAttribute ] [
                    for sequenceType in sequenceTypes do
                        let href = $"?sequenceType={sequenceType.ToString()}"
                        option [
                            _value href
                            if sequenceType = selectedSequenceType then _selected
                        ] [ sequenceType.ToString() |> str ]
                ]
            ]
            div [_class "box-container"] industryBoxes
        ]

    let handler : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
            let sequenceTypeParam = ctx.GetQueryStringValue("sequenceType")
            let sequenceType =
                match sequenceTypeParam with
                | Ok value -> value |> IndustrySequenceType.fromString
                | Error _ -> High
                    
            (Storage.getIndustries()
            |> List.map (Reports.getAllIndustrySMABreakdowns SMA20)
            |> view sequenceType
            |> List.singleton
            |> mainLayout "Industry SMA Breakdown Trends") next ctx
