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
            |> Utils.listOfBusinessDates
            |> Seq.map(fun (date) ->
                let found = mapped.TryFind date.Date
                match found with
                | Some c -> (date,c)
                | None -> (date,0)
            )

        (screener,data)

    let private generateSMATrendRows startDate endDate =

        let smaBreakdowPairs =
            [20; 200]
            |> List.map(fun sma -> 
                (sma, sma |> getDailySMABreakdown startDate endDate)
            )

        let smaDirectionColumns =
            smaBreakdowPairs
            |> List.map(fun (sma, breakdowns) ->
                let trend = TrendsCalculator.calculate breakdowns
                let description = $"<b>SMA {sma}:</b> {trend}"
                [description |> rawText] |> div [_class "column"]   
            )

        let color sma =
            match sma with
            | 20 -> Constants.ColorRed
            | 200 -> Constants.ColorBlue
            | _ -> Constants.ColorBlack

        let datasets:list<Charts.DataSet<decimal>> =
            smaBreakdowPairs
            |> List.map (fun (sma,breakdowns) ->
                {
                    data = breakdowns |> List.map (fun breakdown -> breakdown.percentAboveRounded)
                    title = $"SMA {sma}"
                    color = sma |> color 
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

    let generateFilterSection startDate endDate = 
        let offsets = [-30; -1; 1; 30]

        let buttons = 
            offsets
            |> List.indexed
            |> List.map( fun(index,offset) ->
                button [ _class "button is-primary m-1"; _type "button"; _id $"applyFilters{index}" ] [ str $"{offset}" ]
            )

        let scripts = 
            offsets
            |> List.indexed
            |> List.map( fun(index,offset) ->
                script [ _type "text/javascript" ] [
                    rawText $"document.getElementById('applyFilters{index}').addEventListener('click', function() {{ document.getElementById('dateAdjustment').value = {offset}; document.getElementById('applyFilters').click(); }});"
                ]
            )

        let applyButton = button [ _class "button is-primary m-1"; _type "submit"; _id "applyFilters" ] [ str "Apply" ]

        let formElements = [
            div [_class "field"] [
                label [_class "label"; _for "startDate"] [str "Start Date"]
                input [ _class "input"; _type "date"; _value startDate; _id "startDate"; _name "startDate" ]
            ]
            div [_class "field"] [
                label [_class "label"; _for "endDate"] [str "End Date"]
                input [ _class "input"; _type "date"; _value endDate; _id "endDate"; _name "endDate" ]
            ]
            input [ _class "input"; _type "hidden"; _value ""; _id "dateAdjustment"; _name "dateAdjustment"]
            div [_class "control"] (applyButton::buttons)
        ]

        let form = form [] ([formElements; scripts] |> List.concat)

        div [_class "content"] [
            h4 [] [ str "Filters" ]
            form
        ]

    let generateIndustriesSection date =

        let lastKnownDate =
            date
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
                    [20; 200]
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

                let content = 
                    industryTrendSections
                    |> List.append [
                        h4 [] [str $"Industry Trend Breakdown"]
                        industryTrendBreakdownTable
                    ]

                div [_class "content"] content

    let generateElementsToRender dateRange =
        
        let (startDate,endDate) = dateRange

        let filters = generateFilterSection startDate endDate

        let trendingUpAndDownIndustries = generateIndustriesSection endDate
            
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
            ReportsConfig.dateRange
            |> Utils.listOfBusinessDates
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
            |> Charts.convertNameCountsToChart "Highs - Lows" Charts.Line None Charts.smallChart ReportsConfig.getBackgroundColorDefault
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

        let trends = generateSMATrendRows startDate endDate

        let jobStatusRow = IndustryTrendsJob |> Utils.genericJobStatusGet |> generateJobStatusDiv

        [
            [filters]
            trends
            [trendingUpAndDownIndustries]
            numberOfHitsPartial
            [highsMinusLowsChart]
            volumePartial
            [jobStatusRow]
        ] |> List.concat

    let handler : HttpHandler =

        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
        
            let startDateParam = ctx.TryGetQueryStringValue "startDate"
            let endDateParam = ctx.TryGetQueryStringValue "endDate"
            let dateAdjustmentParam = ctx.TryGetQueryStringValue "dateAdjustment"

            let dateRange = ReportsConfig.dateRangeAsStrings

            let startDate = 
                match startDateParam with
                    | Some s -> s
                    | None -> dateRange |> fst

            let endDate =
                match endDateParam with
                    | Some s -> s
                    | None -> dateRange |> snd

            let dateAdjustmentParamValue =
                match dateAdjustmentParam with
                    | Some s -> s
                    | None -> ""

            let adjustment =
                match dateAdjustmentParamValue with
                    | "" -> 0
                    | _ -> int dateAdjustmentParamValue

            let (adjustedStart, adjustedEnd) =
                match adjustment with
                    | 0 -> (startDate, endDate)  
                    | _ ->
                        let startDate = System.DateTime.Parse(startDate)
                        let endDate = System.DateTime.Parse(endDate)
                        let adjustedStart = startDate.AddDays(adjustment)
                        let adjustedEnd = endDate.AddDays(adjustment)
                        (adjustedStart |> Utils.convertToDateString, adjustedEnd |> Utils.convertToDateString)

            let elementsToRender = generateElementsToRender (adjustedStart,adjustedEnd)

            (elementsToRender |> mainLayout "All Screener Trends") next ctx