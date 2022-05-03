namespace FinvizScraper.Web.Handlers

module ScreenersTrends =

    open Giraffe.ViewEngine
    open FinvizScraper.Web.Shared
    open FinvizScraper.Storage
    
    let handler() =

        let days = FinvizScraper.Core.FinvizConfig.dayRange

        let listOfDays = 
            [-days .. 0]
            |> List.map (fun i -> (System.DateTime.UtcNow.Date.AddDays(i),0))
            |> List.where( fun (date,count) ->
                date.DayOfWeek = System.DayOfWeek.Saturday |> not &&
                date.DayOfWeek = System.DayOfWeek.Sunday |> not
            )
        
        let screeners =
            Storage.getScreeners()
            |> List.where (fun s -> s.id = FinvizScraper.Core.FinvizConfig.NewHighsWithSalesScreener |> not)

        let dataByScreenerByDate =
            screeners
            |> List.map (fun screener -> 
                let mapped = 
                    days
                    |> Reports.getDailyCountsForScreener screener.id
                    |> Map.ofList

                let data =
                    listOfDays
                    |> List.map(fun (date,count) ->
                        let found = mapped.TryFind date
                        match found with
                        | Some c -> (date,c)
                        | None -> (date,count)
                    )

                (screener,data)
            )
            |> Map.ofList

        // make chart that is new highs - new lows for each day

        let newHighsData =
            dataByScreenerByDate.Item(
                screeners
                |> List.find (fun s -> s.id = FinvizScraper.Core.FinvizConfig.NewHighsScreener)
            )
            |> Map.ofList
            
        let newLowsData =
            dataByScreenerByDate.Item(
                screeners
                |> List.find (fun s -> s.id = FinvizScraper.Core.FinvizConfig.NewLowsScreener)
            )
            |> Map.ofList

        let highsMinusLowsChart =
            listOfDays
            |> List.map(fun (date,count) ->
                let high = newHighsData.Item(date)
                let low = newLowsData.Item(date)

                (date,high - low)
            )
            |> Charts.convertNameCountsToChart "Highs - Lows" Charts.smallChart
            |> div [_class "block"]

        let charts =
            dataByScreenerByDate
            |> Map.toList
            |> List.map (fun (screener,screenerData) ->
                
                screenerData
                |> Charts.convertNameCountsToChart screener.name Charts.smallChart
                |> div [_class "block"]                 
            )

        let view = 
            div [_class "content"] [
                h1 [] [
                    str "All Screener Trends"
                ]
            ]::charts

        [highsMinusLowsChart]
            |> List.append view
            |> Views.mainLayout "All Screener Trends"