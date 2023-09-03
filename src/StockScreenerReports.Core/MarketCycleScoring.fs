namespace StockScreenerReports.Core

module MarketCycleScoring =
    
    // TODO: ---> don't forget: it has to be recent, like the last three days, ideally come from low point, like under 20
    // right now I am missing the "bonus" for cycles that start with under 20
    
    type ScoreComponents = { direction:int; age:int; change:int }

    let calculateScoreComponents direction age change =
        let directionComponent = 
            match direction with
            | Down -> -1
            | Up -> 1

        let ageComponent =
            match age with
            | x when x <= 10 -> 10 - age
            | _ -> 0

        let changeComponent =
            match change with
            | x when x > 0m && x >= 40m  -> 10
            | x when x > 0m && x >= 30m  -> 8
            | x when x > 0m && x >= 20m  -> 3
            | x when x > 0m && x > 10m  -> 1
            | _ -> 0

        { direction = directionComponent; age = ageComponent; change = changeComponent }

    let trendScoreComponents (industryBreakdowns:list<IndustrySMABreakdown>) =
        let breakdowns = industryBreakdowns |> List.map (fun x -> x.breakdown)
        
        let trendWithCycle = breakdowns |> TrendsCalculator.calculate

        let trend = trendWithCycle.trend

        calculateScoreComponents trend.direction trend.streak trend.change

    let cycleScoreComponents (industryBreakdowns:list<IndustrySMABreakdown>) =
        let breakdowns = industryBreakdowns |> List.map (fun x -> x.breakdown)
        
        let trendWithCycle = breakdowns |> TrendsCalculator.calculate

        let trend = trendWithCycle.trend
        let cycle = trendWithCycle.cycle

        calculateScoreComponents trend.direction cycle.ageInMarketDays trend.change

    let private componentScoreAdding components =
        components.direction * (components.age + components.change)

    // not sure if I want to use this one 
    // let private componentScoreMultiplying components =
    //     components.direction * components.age * components.change

    let componentScore = componentScoreAdding