namespace StockScreenerReports.Web.Handlers

module Dashboard =

    open Giraffe
    open Giraffe.ViewEngine
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Storage
    open StockScreenerReports.Core
    open StockScreenerReports.Storage.Reports
    open StockScreenerReports.Web.Shared.Views
    open FSharp.Data
    
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
    
    let private generateSMATrendRows smaBreakdownPairs =

        let smaDirectionColumns = smaBreakdownPairs |> generateSMADirectionColumns

        let datasets:list<Charts.DataSet<decimal>> =
            smaBreakdownPairs
            |> List.map (fun (sma:SMA,breakdowns) ->
                {
                    data = breakdowns |> List.map (_.percentAboveRounded)
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
            
        let alertRowDivId (alert:Alert) = $"alert-row-{alert.identifier}"
            
        let mapAlertToDiv alert =
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
            
            div [ _class "columns alert-row"; alert |> alertRowDivId |> _id ] [
                div [_class "column"] screenerNode
                div [_class "column"] industryNode
                div [_class "column is-two-thirds"] [
                    alert.sentiment |> toSentimentSpan
                    alert.description |> str
                ]
                div [_class "column has-text-right"] [
                    button [
                        _class "button is-small is-info acknowledge-button"
                        _onclick $"acknowledgeAlert('{alert.identifier}', '{alert |> alertRowDivId}')"
                    ] [str "Acknowledge"]
                ]
            ]
            
        let renderDateGroup (date, alertsForDate) =
            let alertCount = List.length alertsForDate
            
            let identifierArrayInJavascript = alertsForDate |> List.map (fun (a:Alert) -> $"'{a.identifier}'") |> String.concat ","
            let divIdArrayInJavascript = alertsForDate |> List.map (fun (a:Alert) -> $"'{a |> alertRowDivId}'") |> String.concat ","
            
            div [] [
                div [ _class "columns" ] [
                    div [ _class "column" ] [
                        strong [] [date |> Utils.convertToDateString |> str]
                        span [ _class "tag is-info ml-2" ] [alertCount |> string |> str]
                    ]
                    div [ _class "column has-text-right" ] [
                       button [
                           _class "button is-small is-info"
                           _onclick $"acknowledgeDateAlert([{identifierArrayInJavascript}], [{divIdArrayInJavascript}])"
                       ] [str "Acknowledge All"]
                   ]
                ]
                
                yield!
                    alertsForDate
                    |> List.sortByDescending (fun a -> a.sentiment, a.strength)
                    |> List.map mapAlertToDiv
            ]
            
        div [ _class "content" ] [
        h4 [] ["Alerts" |> str]
        yield!
            match alerts with
            | [] ->
                [div [] ["No alerts" |> str]]
            | _ ->
                alerts
                |> List.groupBy (fun a -> a.date)
                |> List.sortByDescending fst
                |> List.map renderDateGroup
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
        
    let generateElementsToRender missedJobs screeners latestScreenerResults smaBreakdowns cycles industries upAndDowns alerts activeIndustrySequences dateRange =
        
        let warningSection = jobAlertSection missedJobs
        
        let alertsSection = generateAlertsSection screeners alerts
        
        let activeIndustrySequencesSection = generateActiveIndustrySequencesSection activeIndustrySequences

        let filters = generateFilterSection [] dateRange

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

        let trendsSection = smaBreakdowns |> generateSMATrendRows
            
        let cyclesSection = generateIndustryCycleStartChart "Cycle Starts (SMA20)" cycles

        [
            [warningSection]
            [filters]
            trendsSection
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
                |> List.filter (fun job ->
                    match job.name with
                    | EarningsJob | ScreenerJob | TrendsJob | CorporateActionsJob -> true
                    | AlertsJob | CountriesJob -> true
                    | TestJob -> false)
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
            
            let smaBreakdowns =
                SMA.All
                |> List.map(fun sma -> 
                    (sma, sma |> getDailySMABreakdown dateRange)
                )

            let elementsToRender = generateElementsToRender missedJobs screeners latestScreenerResults smaBreakdowns cycles industries upAndDowns alerts activeIndustrySequences dateRange

            (elementsToRender |> mainLayout "All Screener Trends") next ctx
      
      
    type SmaTrendExportType =   CsvProvider<
        Schema = "date(string),sma20",
        HasHeaders=false>
    let smaHeader = "date,sma20"
    
    type CycleExportType =   CsvProvider<
        Schema = "date(string),counts",
        HasHeaders=false>
    let cycleStartsHeader = "date,starts"
    let cycleHighsHeader = "date,highs"
    
    let private exportGeneric filename headerRow data =
        setHttpHeader "Content-Type" "text/csv"
        >=> setHttpHeader "Content-Disposition" $"attachment; filename={filename}"
        >=> setBodyFromString (headerRow + System.Environment.NewLine + data)
    
    let exportSma20Handler() =
        let dateRange = ReportsConfig.dateRangeAsStrings()
            
        let smaBreakdowns =
            SMA.All
            |> List.map(fun sma -> 
                (sma, sma |> getDailySMABreakdown dateRange)
            )
            
        let sma20Data = smaBreakdowns[0] |> snd
        let dates = sma20Data |> List.map (fun b -> b.date |> Utils.convertToDateString)
        
        // csv format should be date,sma20,sma200
        let rows = 
            dates
            |> List.mapi (fun i date ->
                SmaTrendExportType.Row(date,sma20Data[i].percentAbove.ToString())
            )
            
        new SmaTrendExportType(rows) |> _.SaveToString() |> exportGeneric "smaExport.csv" smaHeader
    
    let private exportCycleData filename headerRow groupByfunction =
        let cycles = SMA20 |> Storage.getIndustryCycles
        
        let startPointDateSelector = fun (_, x) -> x.startPoint.date
        let endPointDateSelector = fun (_, x) -> x.currentPoint.date
        
        let cycleStarts = cycles |> List.groupBy (fun (_, c:MarketCycle) -> groupByfunction c) |> Map.ofList
        
        let minStart = cycles |> List.minBy startPointDateSelector |> startPointDateSelector
        let maxStart = cycles |> List.maxBy endPointDateSelector |> endPointDateSelector
        
        let listOfDays = ReportsConfig.listOfBusinessDates (minStart, maxStart)
        
        let dateCounts = 
            listOfDays |> Seq.map (fun (date:System.DateTime) -> 
                let dateFormatted = date.ToString("d")
                let cyclesForDate = cycleStarts |> Map.tryFind dateFormatted
                match cyclesForDate with
                | Some cycles -> CycleExportType.Row(date |> Utils.convertToDateString, cycles.Length.ToString())
                | None -> CycleExportType.Row(date  |> Utils.convertToDateString, "0")
            )
            
        new CycleExportType(dateCounts) |> _.SaveToString() |> exportGeneric filename headerRow
                    
    let exportCycleStartsHandler() =
        exportCycleData "CycleStarts.csv" cycleStartsHeader (_.startPointDateFormatted)
        
    let exportCycleHighsHandler() =
        exportCycleData "CycleHighs.csv" cycleHighsHeader (_.highPointDateFormatted)