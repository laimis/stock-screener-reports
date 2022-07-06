namespace FinvizScraper.Core

    module IndustryTrendsCalculator =
        open System

        let calculate smaBreakdowns =
            
            let mutable latestValue = Option<decimal>.None
            let mutable firstValue = Option<decimal>.None
            let mutable direction = Option<TrendDirection>.None
            let mutable streak = 1
            let mutable endReached = false

            smaBreakdowns
                |> List.sortByDescending (fun b -> b.breakdown.date)
                |> List.iter (fun x ->

                    if endReached then
                        ()
                    else
                        match (latestValue, direction) with
                        // case where we are seeing the first value
                        | (None, None) -> 
                            latestValue <- Some x.breakdown.percentAbove
                            firstValue <- Some x.breakdown.percentAbove
                            // firstDate <- x.breakdown.date

                        // case where we are seeing the second value
                        | (Some _, None) -> (
                            if x.breakdown.percentAbove > latestValue.Value then
                                direction <- Some Down
                                latestValue <- Some x.breakdown.percentAbove
                            else
                                direction <- Some Up
                            )

                        // case where we are now iterating
                        | (Some _, Some _) ->
                            let newDirection =
                                match x.breakdown.percentAbove > latestValue.Value with
                                | true -> Down
                                | false -> Up
                            
                            if newDirection = direction.Value then
                                streak <- streak + 1
                                latestValue <- Some x.breakdown.percentAbove
                                // lastDate <- x.breakdown.date
                            else
                                endReached <- true

                        | (None, Some _) -> raise (new Exception("should not happen where latestValue is None and direction is not"))
                )

            let change = firstValue.Value - latestValue.Value
            
            (streak, direction.Value, change)