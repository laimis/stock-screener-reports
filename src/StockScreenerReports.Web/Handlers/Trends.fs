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
            |> Seq.map(fun (date) ->
                let found = mapped.TryFind date.Date
                match found with
                | Some c -> (date,c)
                | None -> (date,0L)
            )

        (screener,data)

    let internal generateSMADirectionColumns smaBreakdowPairs =
        smaBreakdowPairs
        |> List.map(fun (sma, breakdowns) ->
            let trendWithCycle = TrendsCalculator.calculate breakdowns
            let trendHtml = $"<b>SMA {sma}:</b> {trendWithCycle.trend |> trendToHtml}"
            let cycleHtml = $"Market cycle: {trendWithCycle.cycle |> marketCycleToHtml}"

            div [_class "column"] [
                div [] [trendHtml |> rawText]
                div [] [cycleHtml |> rawText]
            ]   
        )

    let private generateSMATrendRows smaBreakdowPairs =

        let smaDirectionColumns = smaBreakdowPairs |> generateSMADirectionColumns

        let datasets:list<Charts.DataSet<decimal>> =
            smaBreakdowPairs
            |> List.map (fun (sma,breakdowns) ->
                {
                    data = breakdowns |> List.map (fun breakdown -> breakdown.percentAboveRounded)
                    title = $"SMA {sma}"
                    color = sma |> Constants.mapSmaToColor 
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

        let labels = smaBreakdowPairs |> List.head |> snd |> List.map (fun breakdown -> breakdown.date.ToString("MM/dd"))

        [
            div [_class "content"] [h4 [] [ "SMA Trends" |> str ]]
            div [_class "columns"] smaDirectionColumns
            div [_class "block"]
                (Charts.generateChartElements "SMA breakdown" Charts.ChartType.Line (Some 100) Charts.smallChart labels datasets)
            div [_class "block"]
                (Charts.generateChartElements "SMA breakdown (smoothed)" Charts.ChartType.Line (Some 100) Charts.smallChart labels smoothedDatasets)
        ]

    let generateIndustriesSection dateRange =

        let lastKnownDate =
            dateRange
            |> snd
            |> getIndustryTrendsLastKnownDateAsOf

        match lastKnownDate with
            | None ->
                div [ _class "mesage is-danger"] [
                    div [ _class "message-header"] [
                        str "No data found"
                    ]
                    div [ _class "message-body"] [
                        str "No data found for the selected date"
                    ]
                ]
            | Some d ->
                let dateToUse = d |> Utils.convertToDateString

                let upAndDowns = 
                    Constants.SMAS
                    |> List.map (fun days -> days |> getIndustryTrendBreakdown dateToUse)
                
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
                        let trendingIndustries = getTopIndutriesTrending dateToUse 8 direction
                        let topIndustriesTable = 
                            trendingIndustries
                            |> List.map (fun trend ->
                                tr [] [
                                    trend.industry |> Links.industryLink |> generateHref trend.industry |> toTdWithNodeWithWidth 400
                                    trend.trend.streakFormatted |> toTd
                                    trend.trend.changeFormatted |> toTd
                                    trend.trend.streakRateFormatted |> toTd
                                    trend.above.ToString() |> toTd
                                    (trend.above + trend.below).ToString() |> toTd
                                    trend.abovePercentageFormatted() |> toTd
                                ]
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

                let industries = 
                    getIndustrySMABreakdowns Constants.SMA20 dateToUse
                    |> List.map (fun b -> b.industry)

                let scoreRows = 
                    industries
                    |> List.map (fun industry ->
                        let breakdowns = getIndustrySMABreakdownsForIndustry Constants.SMA20 dateRange industry
                        let cycleScoreComponents = MarketCycleScoring.cycleScoreComponents breakdowns
                        let scoreAdd = cycleScoreComponents |> MarketCycleScoring.componentScoreAdding
                        let scoreMult = cycleScoreComponents |> MarketCycleScoring.componentScoreMultiplying
                        let trendScoreComponents = MarketCycleScoring.trendScoreComponents breakdowns
                        let trendScoreAdd = trendScoreComponents |> MarketCycleScoring.componentScoreAdding
                        let trendScoreMult = trendScoreComponents |> MarketCycleScoring.componentScoreMultiplying
                        (industry, scoreAdd, scoreMult, trendScoreAdd, trendScoreMult)
                    )
                    |> List.filter (fun (_, scoreAdd, scoreMult, _, _) -> scoreAdd > 0 || scoreMult > 0)
                    |> List.sortByDescending (fun (_, scoreAdd, scoreMult, _, _) -> scoreAdd + scoreMult)
                    |> List.map (fun (industry, cycleAdd, cycleMult, trendAdd, trendMult) ->
                        tr [] [
                            industry |> Links.industryLink |> generateHref industry |> toTdWithNodeWithWidth 400
                            cycleAdd.ToString() |> toTd
                            cycleMult.ToString() |> toTd
                            trendAdd.ToString() |> toTd
                            trendMult.ToString() |> toTd
                        ]
                    )

                let industryScoresSection =
                    section [] [
                        h4 [] [str "Industry Scores"]
                        scoreRows
                        |> fullWidthTableWithSortableHeaderCells [ "Industry"; "Cycle Add"; "Cycle Mult"; "Trend Add"; "Trend Mult" ]
                    ]


                let content = 
                    industryTrendSections
                    |> List.append [
                        h4 [] [str $"Industry Trend Breakdown"]
                        industryTrendBreakdownTable
                        industryScoresSection
                    ]

                div [_class "content"] content

    let generateElementsToRender dateRange =
        
        // get latest job runs
        let missedJobs =
            Storage.getJobs()
            |> List.filter (fun job -> job.name = TrendsJob)
            |> List.filter Utils.failedJobFilter

        let warningSection = Views.jobAlertSection missedJobs

        let filters = generateFilterSection dateRange

        let trendingUpAndDownIndustries = generateIndustriesSection dateRange
            
        let screeners = Storage.getScreeners()

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
            |> Seq.map(fun (date) ->
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
            Constants.SMAS
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

            let elementsToRender = generateElementsToRender dateRange

            (elementsToRender |> mainLayout "All Screener Trends") next ctx