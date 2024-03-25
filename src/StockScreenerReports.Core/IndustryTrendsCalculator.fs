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
                        | 0m, 0m -> trend.streak + 1
                        | 0m, _ -> 1
                        | _ -> 
                            match direction with
                            | x when x = trend.direction -> trend.streak + 1
                            | _ -> 1

                    let currentPoint = breakdown |> CyclePoint.create

                    let lowPoint = 
                        match cycle.startPointValue with
                        | x when x >= breakdown.percentAbove -> currentPoint
                        | x when x = 0m && currentPoint.value = 0m -> currentPoint
                        | _ -> cycle.startPoint

                    let highPoint =
                        match cycle.highPointValue with
                        | x when x <= breakdown.percentAbove -> currentPoint
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
                        streak = streak; change = finalChange; value = breakdown.percentAbove; direction = direction
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

        let calculateForIndustry (smaBreakdowns:list<IndustrySMABreakdown>) =
            let justBreakdowns = smaBreakdowns |> List.map _.breakdown
            calculate justBreakdowns
            
        let calculateSMACrossOverStrength shortPeriod longPeriod (smaBreakdowns:list<IndustrySMABreakdown>) =
            
            let toSMA (prices:decimal list) interval =
        
                let sma = Array.create prices.Length None    
                for i in 0..prices.Length-1 do
                    if i < interval then
                        sma[i] <- None
                    else
                        let sum = 
                            [i-interval..i-1]
                            |> Seq.map (fun j -> prices[j])
                            |> Seq.sum
                            
                        sma[i] <- Some (System.Math.Round(sum / decimal interval, 2))
                        
                sma
                
            let percentagesAboveSMA =
                smaBreakdowns
                |> List.map (fun breakdown -> breakdown.breakdown.date, breakdown.breakdown.percentAbove)
                |> List.sortBy fst
                |> List.map snd

            let shortSMA = toSMA percentagesAboveSMA shortPeriod |> Array.last |> Option.get
            let longSMA = toSMA percentagesAboveSMA longPeriod |> Array.last |> Option.get

            let trend = if shortSMA > longSMA then 1m else -1m
            let strength = abs (shortSMA - longSMA)

            trend * strength