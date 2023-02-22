namespace StockScreenerReports.Web.Handlers

module ScreenersTrends =

    open Giraffe.ViewEngine
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Storage
    open StockScreenerReports.Core
    
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

        let allElements = List.concat [numberOfHitsPartial; [highsMinusLowsChart]; volumePartial]
        allElements |> Views.mainLayout "All Screener Trends"