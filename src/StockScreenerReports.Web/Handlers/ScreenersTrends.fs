namespace StockScreenerReports.Web.Handlers

module ScreenersTrends =

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

        let sma20 = 20 |> getDailySMABreakdown startDate endDate
        let sma200 = 200 |> getDailySMABreakdown startDate endDate

        let trend20 = TrendsCalculator.calculate sma20
        let trend200 = TrendsCalculator.calculate sma200

        let toDescription (sma:int) (trend:Trend) =
            $"<b>SMA {sma}:</b> {trend}"

        let sma20DirectionDescription = trend20 |> toDescription 20
        let sma200DirectionDescription = trend200 |> toDescription 200

        let datasets:list<Charts.DataSet<decimal>> = [
            {
                data = sma20 |> List.map (fun breakdown -> breakdown.percentAboveRounded)
                title = "SMA 20"
                color = Constants.ColorRed
            };
            {
                data = sma200 |> List.map (fun breakdown -> breakdown.percentAboveRounded)
                title = "SMA 200"
                color = Constants.ColorBlue
            }
        ]

        let labels = sma20 |> List.map (fun breakdown -> breakdown.date.ToString("MM/dd"))

        [
            div [_class "content"] [
                h1 [] [
                    str "SMA Trends"
                ]
            ]
            div [_class "columns"] [
                div [ _class "column" ] [
                    rawText sma20DirectionDescription
                ]
                div [ _class "column" ] [
                    rawText sma200DirectionDescription
                ]
            ]
            div [_class "block"]
                (Charts.generateChartElements "SMA breakdown" Charts.ChartType.Line (Some 100) Charts.smallChart labels datasets)
        ]

    let generateFilterSection startDate endDate = 
        div [_class "content"] [
            h1 [] [
                str "Filters"
            ]
            form [] [
                
                div [_class "columns"] [
                    div [_class "column"] [
                        div [_class "field"] [
                            label [_class "label"; _for "startDate"] [str "Start Date"]    
                        ]
                        input [ _class "input"; _type "date"; _value startDate; _id "startDate"; _name "startDate" ]
                    ]
                    div [_class "column"] [
                        div [_class "field"] [
                            label [_class "label"; _for "endDate"] [str "End Date"]    
                        ]
                        input [ _class "input"; _type "date"; _value endDate; _id "endDate"; _name "endDate" ]
                    ]
                ]
                div [_class "control"] [
                    button [ _class "button is-primary"; _type "submit"; _id "applyFilters" ] [ str "Apply Filters" ]
                ]
            ]
        ]

    let generateIndustriesSection() =
        let (up20,down20) = getIndustryTrendBreakdown 20
        let (up200,down200) = getIndustryTrendBreakdown 200

        let positiveClass = "has-text-success has-text-weight-bold"
        let negativeClass = "has-text-danger has-text-weight-bold"

        let trend20CssClass =
            match up20 >= down20 with
            | true -> positiveClass
            | false -> negativeClass

        let trend200CssClass =
            match up200 >= down200 with
            | true -> positiveClass
            | false -> negativeClass

        let industryTrendBreakdownRow = tr [] [
            td [_class trend20CssClass] [up20.ToString() |> str]
            td [_class trend20CssClass] [down20.ToString() |> str]
            td [_class trend200CssClass] [up200.ToString() |> str]
            td [_class trend200CssClass] [down200.ToString() |> str]
        ]

        let industryTrendBreakdownTable = 
            [ industryTrendBreakdownRow ] |> fullWidthTable [ "20 Up"; "20 Down"; "200 Up"; "200 Down" ]

        let industryTrendSections =
            [(Up, "Industries Trending Up"); (Down, "Industries Trending Down")]
            |> List.map (fun (direction, title) -> 
                let trendingIndustries = getTopIndutriesTrending direction  8
                let topIndustriesTable = 
                    trendingIndustries
                    |> List.map (fun trend ->
                        tr [] [
                            trend.industry |> Links.industryLink |> generateHref trend.industry |> toTdWithNode
                            trend.trend.streakFormatted |> toTd
                            trend.trend.changeFormatted |> toTd
                            trend.trend.streakRateFormatted |> toTd
                        ]
                    )
                    |> List.ofSeq
                    |> fullWidthTable [ "Industry"; "Streak"; "Change"; "Streak Rate" ]
                
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
            |> List.append [h4 [] [str "Industry Trend Breakdown"]; industryTrendBreakdownTable]

        div [_class "content"] content

    let generateElementsToRender dateRange =
        
        let (startDate,endDate) = dateRange

        let filters = generateFilterSection startDate endDate

        let industriesTrendingUp = generateIndustriesSection()
            
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
            FinvizConfig.dateRange
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
            |> Charts.convertNameCountsToChart "Highs - Lows" Charts.Bar None Charts.smallChart FinvizConfig.getBackgroundColorDefault
            |> div [_class "block"]

        let numberOfHitsCharts =
            numberOfHitsByScreenerByDate
            |> Map.toList
            |> List.map (fun (screener,screenerData) ->
                
                screenerData
                |> List.ofSeq
                |> Charts.convertNameCountsToChart screener.name Charts.Bar None Charts.smallChart (FinvizConfig.getBackgroundColorForScreenerId screener.id) 
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
                |> Charts.convertNameCountsToChart screener.name Charts.Bar None Charts.smallChart (FinvizConfig.getBackgroundColorForScreenerId screener.id) 
                |> div [_class "block"]                 
            )

        let volumePartial = 
            div [_class "content"] [
                h1 [] [
                    str "Total Volume for Each Screener"
                ]
            ]::volumeCharts        

        let trends = generateSMATrendRows startDate endDate

        [
            [filters]
            [industriesTrendingUp]
            trends
            numberOfHitsPartial
            [highsMinusLowsChart]
            volumePartial
        ]
        |> List.concat

    let handler : HttpHandler =

        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
        
            let startDateParam = ctx.TryGetQueryStringValue "startDate"
            let endDateParam = ctx.TryGetQueryStringValue "endDate"
            let dateRange = FinvizConfig.dateRangeAsStrings

            let startDate = 
                match startDateParam with
                    | Some s -> s
                    | None -> dateRange |> fst

            let endDate =
                match endDateParam with
                    | Some s -> s
                    | None -> dateRange |> snd

            let elementsToRender = generateElementsToRender (startDate,endDate)

            (elementsToRender |> Views.mainLayout "All Screener Trends") next ctx