namespace StockScreenerReports.Core

module MarketCycleScoring =
    
    type ScoreComponents = { direction:int; age:int; change:int }

    let private calculateScore components =
        components.direction * (components.age + components.change)

    // NOTE: this is a very simple scoring system
    // it is not meant to be used for anything other than a quick and dirty way to rank stocks
    // it is not meant to be used for anything other than a quick and dirty way to rank stocks
    // it is not meant to be used for anything other than a quick and dirty way to rank stocks
    let calculateScoreComponents direction age change =
        let directionComponent = 
            match direction with
            | Down -> -1
            | Up -> 1

        let ageComponent =
            match age with
            | x when x <= 10 -> 10
            | x when x <= 20 -> 8
            | x when x <= 30 -> 3
            | _ -> 0

        let changeComponent =
            match change with
            | x when x > 0m && x >= 40m  -> 10
            | x when x > 0m && x >= 30m  -> 8
            | x when x > 0m && x >= 20m  -> 3
            | x when x > 0m && x > 10m  -> 1
            | _ -> 0

        { direction = directionComponent; age = ageComponent; change = changeComponent }

    let trendScore (industryBreakdowns:list<IndustrySMABreakdown>) =
        let breakdowns = industryBreakdowns |> List.map (fun x -> x.breakdown)
        
        let trendWithCycle = breakdowns |> TrendsCalculator.calculate

        let trend = trendWithCycle.trend

        calculateScoreComponents trend.direction trend.streak trend.change
        |> calculateScore


    let cycleScore (industryBreakdowns:list<IndustrySMABreakdown>) =
        let breakdowns = industryBreakdowns |> List.map (fun x -> x.breakdown)
        
        let trendWithCycle = breakdowns |> TrendsCalculator.calculate

        let trend = trendWithCycle.trend
        let cycle = trendWithCycle.cycle

        calculateScoreComponents trend.direction cycle.ageInMarketDays trend.change
        |> calculateScore