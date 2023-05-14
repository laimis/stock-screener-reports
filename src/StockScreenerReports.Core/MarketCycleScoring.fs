namespace StockScreenerReports.Core

module MarketCycleScoring =
    
    let interestScore (breakdowns:list<SMABreakdown>) =
        
        let trendWithCycle = breakdowns |> TrendsCalculator.calculate

        // it has to be recent, like the last three days, ideally come from low point, like under 20
        let trend = trendWithCycle.trend
        let cycle = trendWithCycle.cycle

        let direction = 
            match trend.direction with
            | Down -> -1
            | Up -> 1

        let age =
            match cycle.age with
            | x when x < System.TimeSpan.FromDays(3.5) -> 10
            | x when x < System.TimeSpan.FromDays(5.0) -> 5
            | _ -> 0


        let change =
            match trend.change with
            | x when x > 0m && x >= 40m  -> 10
            | x when x > 0m && x >= 30m  -> 8
            | x when x > 0m && x >= 20m  -> 3
            | _ -> 0

        direction * (age + change)
    
    let interestScoreForIndustry (breakdowns:list<IndustrySMABreakdown>) =
        breakdowns |> List.map (fun x -> x.breakdown) |> interestScore
    