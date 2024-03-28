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
            
        let calculateSMACrossOverStrengthWithPeriods shortPeriod longPeriod (smaBreakdowns:list<IndustrySMABreakdown>) =
            
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
            
        let calculateSMACrossOverStrength = calculateSMACrossOverStrengthWithPeriods 5 20
            
        let calculateEMACrossOverStrengthWithPeriods shortPeriod longPeriod (smaBreakdowns:list<IndustrySMABreakdown>) =
            let toEMA (prices:decimal list) interval =
        
                let ema =
                    match prices.Length <= interval with
                    | true ->
                        Array.create prices.Length None
                    | false ->
                        let alpha = 2m / (decimal (interval + 1))
                        let initialEMA = System.Math.Round(Seq.averageBy id prices[..(interval - 1)], 2)

                        prices
                        |> List.toArray
                        |> Array.skip interval
                        |> Array.scan
                            (fun prevEMA price ->
                                let newEMA = alpha * price + (1m - alpha) * (prevEMA |> Option.get)
                                System.Math.Round(newEMA, 2) |> Some)
                            (initialEMA |> Some)
                        |> Array.append (Array.create (interval-1) None)
                        
                ema
                
            let percentagesAboveSMA =
                smaBreakdowns
                |> List.map (fun breakdown -> breakdown.breakdown.date, breakdown.breakdown.percentAbove)
                |> List.sortBy fst
                |> List.map snd

            let shortEMA = toEMA percentagesAboveSMA shortPeriod |> Array.last |> Option.get
            let longEMA = toEMA percentagesAboveSMA longPeriod |> Array.last |> Option.get

            let trend = if shortEMA > longEMA then 1m else -1m
            let strength = abs (shortEMA - longEMA)

            trend * strength
            
        let calculateEMACrossOverStrength = calculateEMACrossOverStrengthWithPeriods 3 10
        
        
        let private calculateTrueRange percentages =
            percentages
            |> Seq.pairwise
            |> Seq.map (fun (prev, curr) -> abs (curr - prev))
            |> Seq.toList

        let private calculateDirectionalIndicator (percentages:decimal seq) isPositive =
            percentages
            |> Seq.pairwise
            |> Seq.map (fun (prev, curr) ->
                let diff = curr - prev
                if isPositive then
                    if diff > 0.0m then diff else 0.0m
                else
                    if diff < 0.0m then -diff else 0.0m)
            |> Seq.toList

        let private calculateADX period (percentages:decimal seq) =
            let trueRange = calculateTrueRange percentages
            let positiveMovement = calculateDirectionalIndicator percentages true
            let negativeMovement = calculateDirectionalIndicator percentages false

            let trAverages = trueRange |> Seq.windowed period |> Seq.map Seq.average |> Seq.toList
            let pmAverages = positiveMovement |> Seq.windowed period |> Seq.map Seq.average |> Seq.toList
            let nmAverages = negativeMovement |> Seq.windowed period |> Seq.map Seq.average |> Seq.toList

            let diPlusList = List.map2 (fun t1 t2 -> match t1, t2 with | 0m, 0m -> 0m | _, 0m -> 0m | 0m, _ -> 0m | _, _ -> t1/t2 ) pmAverages trAverages
            let diMinusList = List.map2 (fun t1 t2 -> match t1, t2 with | 0m, 0m -> 0m | _, 0m -> 0m | 0m, _ -> 0m | _, _ -> t1/t2 ) nmAverages trAverages

            let diDiffList = List.map2 (-) diPlusList diMinusList
            let diSumList = List.map2 (+) diPlusList diMinusList
            let dxList = List.map2 (fun t1 t2 -> match t1, t2 with | 0m, 0m -> 0m | _, 0m -> 0m | 0m, _ -> 0m | _, _ -> t1/t2 ) (List.map abs diDiffList) diSumList

            let adx = dxList |> Seq.windowed period |> Seq.map Seq.average |> Seq.tryLast

            match adx with
            | Some value -> value * 100.0m
            | None -> 0.0m

        let calculateADXTrendWithPeriod period (smaBreakdowns:IndustrySMABreakdown seq) =
            let percentagesAboveSMA = smaBreakdowns |> Seq.map (_.breakdown.percentAbove)
            
            let adx = System.Math.Round(percentagesAboveSMA |> calculateADX period, 2)

            let strength = 
                if Seq.last percentagesAboveSMA > Seq.head percentagesAboveSMA then adx * 1m else adx * -1m

            strength
            
        let calculateADXTrend (smaBreakdowns:IndustrySMABreakdown seq) = calculateADXTrendWithPeriod 10 smaBreakdowns