namespace StockScreenerReports.Web.Handlers

module ScreenersTrends =

    open Giraffe.ViewEngine
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Storage
    open StockScreenerReports.Core
    open StockScreenerReports.Storage.Reports
    
    let private getScreenerDailyHits screener =
        FinvizConfig.dayRange
            |> Reports.getDailyCountsForScreener screener.id

    let private getScreenerDailyAverageVolume screener =
        FinvizConfig.dayRange
            |> Reports.getDailyAverageVolumeForScreener screener.id

    let private getSCreenerDailyTotalVolume screener =
        FinvizConfig.dayRange
            |> Reports.getDailyTotalVolumeForScreener screener.id

    let private getScreenerCountMapByDate screener screenerDataSource =
        let mapped = 
            screener
            |> screenerDataSource
            |> Map.ofList

        let data =
            FinvizConfig.dayRange
            |> Utils.listOfBusinessDates System.DateTime.UtcNow
            |> List.map(fun (date) ->
                let found = mapped.TryFind date
                match found with
                | Some c -> (date,c)
                | None -> (date,0)
            )

        (screener,data)

    let private generateSMATrendRows() =

        let sma20 = FinvizConfig.dayRange |> getDailySMABreakdown 20
        let sma200 = FinvizConfig.dayRange |> getDailySMABreakdown 200

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
            
    let handler() =
        
        let screeners = Storage.getScreeners()

        let numberOfHitsByScreenerByDate =
            screeners
            |> List.map (fun s -> getScreenerCountMapByDate s getScreenerDailyHits)
            |> Map.ofList

        let volumeByScreenerByDate =
            screeners
            |> List.map (fun s -> getScreenerCountMapByDate s getSCreenerDailyTotalVolume)
            |> Map.ofList

        let findScreener screenerId =
            screeners
            |> List.find (fun s -> s.id = screenerId)

        // make chart that is new highs - new lows for each day
        let newHighsDataMap =
            numberOfHitsByScreenerByDate.Item(Constants.NewHighsScreenerId |> findScreener)
            |> Map.ofList
            
        let newLowsDataMap =
            numberOfHitsByScreenerByDate.Item(Constants.NewLowsScreenerId |> findScreener)
            |> Map.ofList

        let highsMinusLowsChart =
            FinvizConfig.dayRange
            |> Utils.listOfBusinessDates System.DateTime.UtcNow
            |> List.map(fun (date) ->
                let high = newHighsDataMap.Item(date)
                let low = newLowsDataMap.Item(date)

                (date,high - low)
            )
            |> Charts.convertNameCountsToChart "Highs - Lows" Charts.Bar None Charts.smallChart FinvizConfig.getBackgroundColorDefault
            |> div [_class "block"]

        let numberOfHitsCharts =
            numberOfHitsByScreenerByDate
            |> Map.toList
            |> List.map (fun (screener,screenerData) ->
                
                screenerData
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
                |> Charts.convertNameCountsToChart screener.name Charts.Bar None Charts.smallChart (FinvizConfig.getBackgroundColorForScreenerId screener.id) 
                |> div [_class "block"]                 
            )

        let volumePartial = 
            div [_class "content"] [
                h1 [] [
                    str "Total Volume for Each Screener"
                ]
            ]::volumeCharts        

        let trends = generateSMATrendRows()

        [
            trends
            numberOfHitsPartial
            [highsMinusLowsChart]
            volumePartial
        ]
        |> List.concat
        |> Views.mainLayout "All Screener Trends"