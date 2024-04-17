namespace StockScreenerReports.Web.Handlers

module Dashboard =

    open Giraffe
    open Giraffe.ViewEngine
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Storage
    open StockScreenerReports.Core
    open StockScreenerReports.Storage.Reports
    open StockScreenerReports.Web.Shared.Views
    
    let private getScreenerDailyHits (dateRange:string * string) screener =
        screener.id |> getDailyCountsForScreener dateRange

    let private getScreenerDailyAverageVolume(dateRange:string*string) screener =
        screener.id |> getDailyAverageVolumeForScreener dateRange

    let private getScreenerDailyTotalVolume (dateRange:string * string) screener =
        screener.id |> getDailyTotalVolumeForScreener dateRange

    let private getScreenerCountMapByDate screener dateRange screenerDataSource =
        let mapped = 
            screener
            |> screenerDataSource dateRange
            |> Map.ofList

        let parsedDateRange = (
            System.DateTime.Parse(dateRange |> fst),
            System.DateTime.Parse(dateRange |> snd)
        )

        let data =
            parsedDateRange
            |> ReportsConfig.listOfBusinessDates
            |> Seq.map(fun date ->
                let found = mapped.TryFind date.Date
                match found with
                | Some c -> (date,c)
                | None -> (date,0L)
            )

        (screener,data)
    
    let internal generateSMADirectionColumns smaBreakdownPairs =
        smaBreakdownPairs
        |> List.map(fun (sma, breakdowns) ->
            let trendWithCycle = TrendsCalculator.calculate breakdowns
            let trendHtml = $"<b>SMA {sma}:</b> {trendWithCycle.trend |> trendToHtml}"
            let cycleHtml = $"{trendWithCycle.cycle |> marketCycleToHtml}"

            div [_class "column"] [
                div [] [trendHtml |> rawText]
                div [] [cycleHtml |> rawText]
            ]   
        )
        
    let private generateIndustryCycleStartChart (cycles:IndustryWithCycle list) =

        match cycles with
        | [] -> 
            toSectionWithNoContent "No cycles found" 
        | _ ->
            let cyclesGroupedByDate =
                cycles
                |> List.groupBy (fun (_, x) -> x.startPointDateFormatted)
                |> Map.ofList

            let startPointDateSelector = fun (_, x) -> x.startPoint.date
            let minStart = cycles |> List.minBy startPointDateSelector |> startPointDateSelector
            let maxStart = ReportsConfig.now().Date

            let dateCounts = 
                ReportsConfig.listOfBusinessDates (minStart, maxStart)
                |> Seq.map (fun date -> 
                    let dateFormatted = date.ToString("d")
                    let cyclesForDate = cyclesGroupedByDate |> Map.tryFind dateFormatted
                    match cyclesForDate with
                    | Some cycles -> (date, decimal cycles.Length)
                    | None -> (date, 0m)
                )

            let dataset:Charts.DataSet<decimal> =
                {
                    data = dateCounts |> Seq.map snd |> List.ofSeq
                    title = $"start counts"
                    color = Constants.ColorRed
                }

            let maxValue = (dateCounts |> Seq.map snd |> Seq.max) + 5m |> int

            let labels = dateCounts |> Seq.map (fun (date,_) -> date.ToString("MM/dd"))
            let chart = [dataset] |> Charts.generateChartElements "Start counts" Charts.ChartType.Bar (Some maxValue) Charts.smallChart labels

            div [] chart |> toSection "Industry Cycle Start Counts"

    let private generateSMATrendRows smaBreakdownPairs =

        let smaDirectionColumns = smaBreakdownPairs |> generateSMADirectionColumns

        let datasets:list<Charts.DataSet<decimal>> =
            smaBreakdownPairs
            |> List.map (fun (sma:SMA,breakdowns) ->
                {
                    data = breakdowns |> List.map (fun breakdown -> breakdown.percentAboveRounded)
                    title = $"SMA {sma}"
                    color = sma.Color 
                }
            )

        let labels =
            smaBreakdownPairs
            |> List.head
            |> snd
            |> List.map (fun breakdown -> breakdown.date.ToString("MM/dd"))

        [
            div [_class "content"] [h4 [] ["SMA Trends" |> str]]
            div [_class "columns"] smaDirectionColumns
            div [_class "block"]
                (Charts.generateChartElements "SMA breakdown" Charts.ChartType.Line (Some 100) Charts.smallChart labels datasets)
        ]

    let generateIndustriesSection industries upAndDowns =

        match upAndDowns with
        | None ->
            div [ _class "message is-danger"] [
                    div [ _class "message-header"] [
                        str "No data found"
                    ]
                    div [ _class "message-body"] [
                        str "No data found for the selected date"
                    ]
                ]
        | Some upAndDowns ->
            let positiveClass = "has-text-success has-text-weight-bold"
            let negativeClass = "has-text-danger has-text-weight-bold"

            let createCells upAndDown =
                let up = upAndDown |> fst
                let down = upAndDown |> snd

                let cssClass =
                    match up >= down with
                    | true -> positiveClass
                    | false -> negativeClass

                [up; down] |> List.map (fun value -> td [_class cssClass] [value.ToString() |> str])

            let cells = upAndDowns |> List.map createCells |> List.concat

            let industryTrendBreakdownRow = tr [ ] cells

            let industryTrendBreakdownTable = 
                [ industryTrendBreakdownRow ] |> fullWidthTableTextCentered [ "20 Up"; "20 Down"; "200 Up"; "200 Down" ]

            let industryTrendSections =
                [(Up, "Industries Trending Up"); (Down, "Industries Trending Down")]
                |> List.map (fun (direction, title) ->
                    
                    let trendScore (t:IndustryTrend) = t.trend.change/(t.trend.streak |> decimal)
                    
                    let func =
                        match direction with
                        | Up -> fun (s:IndustryTrend list) -> s |> List.sortByDescending trendScore
                        | Down -> fun (s:IndustryTrend list) -> s |> List.sortBy trendScore
                    
                    let topIndustries =
                        match industries with
                        | None -> []
                        | Some industries -> industries |> func |> List.truncate 8
                        
                    let topIndustriesTable = 
                        topIndustries
                        |> List.map (fun trend ->
                            [
                                LinkNewTabColumn(trend.industry, trend.industry |> Links.industryLink)
                                StringColumn(trend.trend.streakFormatted)
                                StringColumn(trend.trend.changeFormatted)
                                StringColumn(trend.trend.streakRateFormatted)
                                StringColumn(trend.above.ToString())
                                StringColumn((trend.above + trend.below).ToString())
                                StringColumn(trend.percentAboveFormatted)
                            ] |> toTr
                        )
                        |> List.ofSeq
                        |> fullWidthTable [ "Industry"; "Streak"; "Change"; "Streak Rate"; "Above"; "Total"; "%";  ]
                    
                    [
                        h4 [] [
                            title |> str
                        ]
                        topIndustriesTable
                    ]
                )
                |> List.concat

            let content = 
                industryTrendSections
                |> List.append [
                    h4 [] [str $"Industry Trend Breakdown"]
                    industryTrendBreakdownTable
                ]

            div [_class "content"] content
            
    let private generateAlertsSection screeners alerts =
        
        let toSentimentSpan sentiment =
            span [sentiment |> sentimentClass |> _class] [
                sentiment |> sentimentText  |> str
            ]
            
        div [ _class "content" ] [
            h4 [] ["Alerts" |> str]
            yield!
                match alerts with
                | [] ->
                    [div [] ["No alerts" |> str]]
                | _ ->
                    alerts
                    |> List.map (fun (alert:Alert) ->
                        
                        let industry = Alert.getIndustry alert
                        let industryNode =
                            match industry with
                            | Some i -> [generateHrefNewTab i (Links.industryLink i)]
                            | None -> []
                            
                        let screenerId = Alert.getScreenerId alert
                        let screenerNode =
                            match screenerId with
                            | Some screenerId ->
                                let screenerName = screeners |> List.find (fun s -> s.id = screenerId) |> _.name
                                [ generateHrefNewTab screenerName (Links.screenerResultsLink screenerId alert.date) ]
                            | None -> []
                        
                        div [ _class "columns" ] [
                            div [_class "column"] screenerNode
                            div [_class "column"] industryNode
                            div [_class "column is-two-thirds"] [
                                alert.sentiment |> toSentimentSpan
                                alert.description |> str
                            ]
                        ]
                    )
        ]
        
    let private generateActiveIndustrySequencesSection (sequences:IndustrySequence list) =
        let toSentimentSpan sequenceType =
            let sentiment = match sequenceType with | High -> Positive | Low -> Negative
            
            span [sentiment |> sentimentClass |> _class] [
                sentiment |> sentimentText  |> str
            ]
        
        div [ _class "content" ] [
            h4 [ _class "mt-5" ] ["Active Industry Sequences" |> str]
            yield!
                match sequences with
                | [] ->
                    [div [] ["No active sequences" |> str]]
                | _ ->
                    sequences
                    |> List.map (fun (sequence:IndustrySequence) ->
                        div [ _class "columns" ] [
                            div [_class "column"] [ generateHrefNewTab sequence.industry (Links.industryLink sequence.industry) ]
                            div [_class "column is-flex is-align-items-center"] (
                                (sequence.type' |> toSentimentSpan) :: (sequence |> sequenceToDurationBarChart)
                            )
                        ]
                    )
                ]
        
    let private generateScreenersSection screenerResults =
        
        let generateScreenerResultSection (screener:ScreenerResultReport) = 
            div [_class "column is-one-quarter"] [
                a [
                    _class "button is-primary is-fullwidth"
                    _style "justify-content: left;"
                    _href (Links.screenerResultsLink screener.screenerid screener.date)] [
                    span [
                        _style "font-size: 1.5em; font-weight: bold; padding-right: 10px"
                    ] [
                        screener.count.ToString() |> str
                    ]
                    $"{screener.name}" |> str
                ]
            ]
            
        let generateRefreshButton() =
            let link = Links.jobsScreeners
            let title = "Refresh"
            div [_class "column is-one-quarter"] [
                a [
                    _class "button is-light is-fullwidth"
                    _style "justify-content: left;"
                    _href link] [
                    title |> str
                ]
            ]
            
        div [_class "content"] [
            h4 [] ["Latest Screener Results" |> str]
            div [_class "columns is-multiline"]
                ((screenerResults |> List.map generateScreenerResultSection) @ [
                    generateRefreshButton()
                ])
        ]
        
    let generateElementsToRender missedJobs screeners latestScreenerResults cycles industries upAndDowns alerts activeIndustrySequences dateRange =
        
        let warningSection = jobAlertSection missedJobs
        
        let alertsSection = generateAlertsSection screeners alerts
        
        let activeIndustrySequencesSection = generateActiveIndustrySequencesSection activeIndustrySequences

        let filters = generateFilterSection dateRange

        let trendingUpAndDownIndustries = generateIndustriesSection industries upAndDowns
        
        let latestScreenerResults = generateScreenersSection latestScreenerResults

        let numberOfHitsByScreenerByDate =
            screeners
            |> List.map (fun s -> getScreenerCountMapByDate s dateRange getScreenerDailyHits)
            |> Map.ofList

        let volumeByScreenerByDate =
            screeners
            |> List.map (fun s -> getScreenerCountMapByDate s dateRange getScreenerDailyTotalVolume)
            |> Map.ofList

        let findScreener screenerId =
            screeners
            |> List.find (fun s -> s.id = screenerId)

        // make chart that is new highs - new lows for each day
        let newHighsDataMap =
            numberOfHitsByScreenerByDate.Item(Constants.NewHighsScreenerId |> findScreener)
            |> Map.ofSeq
            
        let newLowsDataMap =
            numberOfHitsByScreenerByDate.Item(Constants.NewLowsScreenerId |> findScreener)
            |> Map.ofSeq

        let highsMinusLowsChart =
            dateRange
            |> fun (start, ``end``) -> (System.DateTime.Parse(start), System.DateTime.Parse(``end``))
            |> ReportsConfig.listOfBusinessDates
            |> Seq.map(fun date ->
                let highMatch = newHighsDataMap |> Map.tryFind date.Date
                let lowMatch = newLowsDataMap |> Map.tryFind date.Date
                
                match highMatch, lowMatch with
                | Some high, Some low -> Some (date, high - low)
                | _ -> None
            )
            |> Seq.choose id
            |> List.ofSeq
            |> Charts.convertNameCountsToChart "Highs - Lows" Charts.Bar None Charts.smallChart ReportsConfig.getBackgroundColorDefault
            |> div [_class "block"]

        let numberOfHitsCharts =
            numberOfHitsByScreenerByDate
            |> Map.toList
            |> List.map (fun (screener,screenerData) ->
                
                screenerData
                |> List.ofSeq
                |> Charts.convertNameCountsToChart screener.name Charts.Bar None Charts.smallChart (ReportsConfig.getBackgroundColorForScreenerId screener.id) 
                |> div [_class "block"]                 
            )

        let numberOfHitsPartial = 
            div [_class "content"] [
                h1 [] [
                    str "Number of Hits for Each Screener"
                ]
            ]::numberOfHitsCharts

        let volumeCharts =
            volumeByScreenerByDate
            |> Map.toList
            |> List.map (fun (screener,screenerData) ->
                
                screenerData
                |> List.ofSeq
                |> Charts.convertNameCountsToChart screener.name Charts.Bar None Charts.smallChart (ReportsConfig.getBackgroundColorForScreenerId screener.id) 
                |> div [_class "block"]                 
            )

        let volumePartial = 
            div [_class "content"] [
                h1 [] [
                    str "Total Volume for Each Screener"
                ]
            ]::volumeCharts        

        let trends =
            SMA.All
            |> List.map(fun sma -> 
                (sma, sma |> getDailySMABreakdown dateRange)
            )
            |> generateSMATrendRows
            
        let cyclesSection = generateIndustryCycleStartChart cycles

        [
            [warningSection]
            [filters]
            trends
            [cyclesSection]
            [latestScreenerResults]
            [alertsSection]
            [activeIndustrySequencesSection]
            [trendingUpAndDownIndustries]
            numberOfHitsPartial
            [highsMinusLowsChart]
            volumePartial
        ] |> List.concat

    let handler : HttpHandler =

        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
        
            let dateRange = getFilterSectionParams ctx
            
            // get latest job runs
            let missedJobs =
                Storage.getJobs()
                |> List.filter (fun job -> job.name = EarningsJob || job.name = ScreenerJob || job.name = TrendsJob)
                |> List.filter Utils.failedJobFilter
                
            let screeners = Storage.getScreeners()
            
            let lastKnownDate =
                dateRange
                |> snd
                |> getIndustryTrendsLastKnownDateAsOf

            let upAndDowns =
                match lastKnownDate with
                | None ->
                    None
                | Some d ->
                    let dateToUse = d |> Utils.convertToDateString
                    SMA.All
                    |> List.map (fun days -> days |> getIndustryTrendBreakdown dateToUse)
                    |> Some
                    
            let industries = 
                match lastKnownDate with
                | None ->
                    None
                | Some d ->
                    let dateToUse = d |> Utils.convertToDateString
                    SMA20 |> getIndustryTrends dateToUse |> Some
                    
            let latestScreenerResults = getLatestScreenerResults()
                
            let alerts = Storage.getAlerts()
            
            let activeIndustrySequences = Storage.getActiveIndustrySequences()
            
            let cycles = SMA20 |> Storage.getIndustryCycles

            let elementsToRender = generateElementsToRender missedJobs screeners latestScreenerResults cycles industries upAndDowns alerts activeIndustrySequences dateRange

            (elementsToRender |> mainLayout "All Screener Trends") next ctx