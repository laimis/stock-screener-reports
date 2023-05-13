namespace StockScreenerReports.Core

    module TrendsCalculator =
        open System

        type TrendHelper =
            {
                firstValue: decimal;
                latestValue: decimal;
                calculatedTrend: Trend;
            }

        let calculate (smaBreakdowns:list<SMABreakdown>) : Trend =

            let folder trend (breakdown:SMABreakdown) =
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

                {trend with streak = streak; change = finalChange; value = breakdown.percentAbove; direction = direction}
            
            let initValue = {streak = 0; direction = Up; change = 0m; value = smaBreakdowns.Head.percentAbove}

            let trend =
                smaBreakdowns
                |> List.skip 1
                |> List.fold folder initValue

            match trend.streak with
            | 0 -> {trend with streak = 1}
            | _ -> trend

        let calculateForIndustry (smaBreakdowns:list<IndustrySMABreakdown>) =
            let justBreakdowns = smaBreakdowns |> List.map (fun x -> x.breakdown)
            calculate justBreakdowns