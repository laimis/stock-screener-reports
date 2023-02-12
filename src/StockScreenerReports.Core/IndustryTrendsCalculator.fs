namespace StockScreenerReports.Core

    module IndustryTrendsCalculator =
        open System

        let calculate (smaBreakdowns:list<SMABreakdown>) =

            let mutable latestValue = Option<decimal>.None
            let mutable firstValue = Option<decimal>.None
            let mutable direction = Option<TrendDirection>.None
            let mutable streak = 1
            let mutable endReached = false

            smaBreakdowns
                |> List.sortByDescending (fun b -> b.date)
                |> List.iter (fun x ->

                    if endReached then
                        ()
                    else
                        match (latestValue, direction) with
                        // case where we are seeing the first value
                        | (None, None) -> 
                            latestValue <- Some x.percentAbove
                            firstValue <- Some x.percentAbove
                            // firstDate <- x.breakdown.date

                        // case where we are seeing the second value
                        | (Some _, None) -> (
                            if x.percentAbove > latestValue.Value then
                                direction <- Some Down
                                latestValue <- Some x.percentAbove
                            else
                                direction <- Some Up
                            )

                        // case where we are now iterating
                        | (Some _, Some _) ->
                            let newDirection =
                                match x.percentAbove > latestValue.Value with
                                | true -> Down
                                | false -> Up
                            
                            if newDirection = direction.Value then
                                streak <- streak + 1
                                latestValue <- Some x.percentAbove
                                
                                if latestValue.Value = 0m then
                                    endReached <- true
                                else
                                    ()
                            else
                                endReached <- true

                        | (None, Some _) -> raise (new Exception("should not happen where latestValue is None and direction is not"))
                )

            let change = firstValue.Value - latestValue.Value
            
            match direction with
            | Some s -> (streak, s, change)
            | None -> (0, Up, 0m)

        let calculateForIndustry (smaBreakdowns:list<IndustrySMABreakdown>) =
            
            let justBreakdowns = smaBreakdowns |> List.map (fun x -> x.breakdown)

            calculate justBreakdowns