namespace StockScreenerReports.Core

module MarketCycleScoring =
    
    type ScoreComponents = { direction:int; age:int; change:int }

    let interestScoreComponents (industryBreakdowns:list<IndustrySMABreakdown>) =
        let breakdowns = industryBreakdowns |> List.map (fun x -> x.breakdown)
        
        let trendWithCycle = breakdowns |> TrendsCalculator.calculate

        // TODO: ---> don't forget: it has to be recent, like the last three days, ideally come from low point, like under 20

        let trend = trendWithCycle.trend
        let cycle = trendWithCycle.cycle

        let direction = 
            match trend.direction with
            | Down -> -1
            | Up -> 1

        let age =
            match cycle.ageInMarketDays with
            | x when x > 1 && x <= 3 -> 10
            | x when x > 1 && x <= 7 -> 5
            | _ -> 0


        let change =
            match trend.change with
            | x when x > 0m && x >= 40m  -> 10
            | x when x > 0m && x >= 30m  -> 8
            | x when x > 0m && x >= 20m  -> 3
            | _ -> 0

        { direction = direction; age = age; change = change }


    
    let private interestScore componentFunction breakdowns =
        let components = interestScoreComponents breakdowns
        componentFunction components

    let interestScoreAdding =
        let func components = components.direction * (components.age + components.change)
        interestScore func

    let interestScoreMultiplying =
        let func components = components.direction * components.age * components.change
        interestScore func