namespace StockScreenerReports.Web.Handlers

module Trends =

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
            let cycleHtml = $"Market cycle: {trendWithCycle.cycle |> marketCycleToHtml}"

            div [_class "column"] [
                div [] [trendHtml |> rawText]
                div [] [cycleHtml |> rawText]
            ]   
        )

    let private generateSMATrendRows smaBreakdownPairs =

        let smaDirectionColumns = smaBreakdownPairs |> generateSMADirectionColumns

        let datasets:list<Charts.DataSet<decimal>> =
            smaBreakdownPairs
            |> List.map (fun (sma,breakdowns) ->
                {
                    data = breakdowns |> List.map (fun breakdown -> breakdown.percentAboveRounded)
                    title = $"SMA {sma}"
                    color = sma |> SMA.toColor 
                }
            )

        let smoothedDatasets = 
            datasets
            |> List.map (fun d ->
                let windowed =
                    d.data
                    |> List.windowed 3
                    |> List.map (fun u -> u |> List.average |> System.Math.Round)

                { d with data = windowed; title = d.title + " (smoothed)"}
            )

        let labels =
            smaBreakdownPairs
            |> List.head
            |> snd
            |> List.map (fun breakdown -> breakdown.date.ToString("MM/dd"))

        [
            div [_class "content"] [h4 [] [ "SMA Trends" |> str ]]
            div [_class "columns"] smaDirectionColumns
            div [_class "block"]
                (Charts.generateChartElements "SMA breakdown" Charts.ChartType.Line (Some 100) Charts.smallChart labels datasets)
            div [_class "block"]
                (Charts.generateChartElements "SMA breakdown (smoothed)" Charts.ChartType.Line (Some 100) Charts.smallChart labels smoothedDatasets)
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
                    
                    let func =
                        match direction with
                        | Up -> fun (s:IndustryTrend list) -> s |> List.sortByDescending (fun t -> t.trend.change/(t.trend.streak |> decimal))
                        | Down -> fun (s:IndustryTrend list) -> s |> List.sortBy (fun t -> t.trend.change/(t.trend.streak |> decimal))
                        
                    
                    let topIndustries =
                        match industries with
                        | None -> []
                        | Some industries -> industries |> func |> List.take 8
                        
                    let topIndustriesTable = 
                        topIndustries
                        |> List.map (fun trend ->
                            [
                                LinkColumn(trend.industry, trend.industry |> Links.industryLink)
                                StringColumn(trend.trend.streakFormatted)
                                StringColumn(trend.trend.changeFormatted)
                                StringColumn(trend.trend.streakRateFormatted)
                                StringColumn(trend.above.ToString())
                                StringColumn((trend.above + trend.below).ToString())
                                StringColumn(trend.abovePercentageFormatted())
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

    let generateElementsToRender missedJobs screeners industries upAndDowns dateRange =
        
        let warningSection = jobAlertSection missedJobs

        let filters = generateFilterSection dateRange

        let trendingUpAndDownIndustries = generateIndustriesSection industries upAndDowns

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
            ReportsConfig.dateRange()
            |> ReportsConfig.listOfBusinessDates
            |> Seq.map(fun date ->
                let high = 
                    match (newHighsDataMap |> Map.tryFind date.Date)
                    with
                        | Some c -> c
                        | None -> 0
                
                let low =
                    match (newLowsDataMap |> Map.tryFind date.Date)
                    with
                        | Some c -> c
                        | None -> 0

                (date,high - low)
            )
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
            SMA.all
            |> List.map(fun sma -> 
                (sma, sma |> getDailySMABreakdown dateRange)
            )
            |> generateSMATrendRows

        [
            [warningSection]
            [filters]
            trends
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
                |> List.filter (fun job -> job.name = TrendsJob)
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
                    SMA.all
                    |> List.map (fun days -> days |> getIndustryTrendBreakdown dateToUse)
                    |> Some
                    
            let industries = 
                match lastKnownDate with
                | None ->
                    None
                | Some d ->
                    let dateToUse = d |> Utils.convertToDateString
                    SMA20 |> getIndustryTrends dateToUse |> Some

            let elementsToRender = generateElementsToRender missedJobs screeners industries upAndDowns dateRange

            (elementsToRender |> mainLayout "All Screener Trends") next ctx