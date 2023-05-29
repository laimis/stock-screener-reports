namespace StockScreenerReports.Core

    module TrendsCalculator =

        let calculate (smaBreakdowns:list<SMABreakdown>) : TrendWithCycle =

            let folder (trendCycleOption:option<TrendWithCycle>) (breakdown:SMABreakdown) =
                 
                match trendCycleOption with
                | None ->
                    let trend = {streak = 0; direction = Up; change = 0m; value = breakdown.percentAbove}
                    let cycle = {
                        startPoint = breakdown |> CyclePoint.create;
                        highPoint = breakdown |> CyclePoint.create;
                        currentPoint = breakdown |> CyclePoint.create;
                    }

                    Some {
                        trend = trend;
                        cycle = cycle;
                    }
                | Some trendCycle -> 
                    let trend = trendCycle.trend
                    let cycle = trendCycle.cycle

                    let delta = breakdown.percentAbove - trend.value
                    let change = trend.change + delta

                    let direction = 
                        match delta with
                        | x when x > 0m -> Up
                        | x when x < 0m -> Down
                        | _ -> trend.direction

                    // if direction has changed, change is just the delta
                    let finalChange = 
                        match direction with
                        | x when x = trend.direction -> change
                        | _ -> delta

                    // if the trend has flatlined and sitting on zero, we want each day
                    // to reset the streak
                    let streak = 
                        match (trend.value, breakdown.percentAbove) with
                        | (0m, 0m) -> trend.streak + 1
                        | (0m, _) -> 1
                        | _ -> 
                            match direction with
                            | x when x = trend.direction -> trend.streak + 1
                            | _ -> 1

                    let currentPoint = breakdown |> CyclePoint.create

                    let lowPoint = 
                        match cycle.startPointValue with
                        | x when x > breakdown.percentAbove -> currentPoint
                        | x when x = 0m && currentPoint.value = 0m -> currentPoint
                        | _ -> cycle.startPoint

                    let highPoint =
                        match cycle.highPointValue with
                        | x when x < breakdown.percentAbove -> currentPoint
                        | _ ->
                            match cycle.highPointDate < lowPoint.date with
                            | true -> currentPoint
                            | false -> cycle.highPoint

                    let newCycle = {
                        startPoint = lowPoint;
                        highPoint = highPoint;
                        currentPoint = currentPoint;
                    }

                    let newTrend = {
                        trend with streak = streak; change = finalChange; value = breakdown.percentAbove; direction = direction
                    }
                    
                    Some {
                        trend = newTrend;
                        cycle = newCycle;
                    }
            
            let trendCycleOption =
                smaBreakdowns
                |> List.fold folder None

            match trendCycleOption.Value.trend.streak with
            | 0 -> {
                    cycle = trendCycleOption.Value.cycle;
                    trend = {trendCycleOption.Value.trend with streak = 1}
                }
            | _ -> trendCycleOption.Value

        let calculateTrendAndCycleForIndustry (smaBreakdowns:list<IndustrySMABreakdown>) =
            let justBreakdowns = smaBreakdowns |> List.map (fun x -> x.breakdown)
            calculate justBreakdowns

        let calculateForIndustry (smaBreakdowns:list<IndustrySMABreakdown>) =
            let trendWithCycle = smaBreakdowns |> calculateTrendAndCycleForIndustry
            trendWithCycle.trend