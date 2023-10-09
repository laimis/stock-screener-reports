namespace StockScreenerReports.Core

module MarketCycleScoring =
    
    // NOTE: this is a very simple scoring system, I keep on tweaking this and not
    // happy with what it gives back. Right now the best way to judge the industry
    // is to look at the SMA trend chart and use manual visual inspection.
    // This helps a bit to filter industries but still top result might not be top
    // industry.
    let private calculateScore value direction age change =
        let directionComponent = 
            match direction with
            | Down -> -1
            | Up -> 1

        // let ageComponent =
        //     match age with
        //     | x when x <= 10 -> 10
        //     | x when x <= 20 -> 8
        //     | x when x <= 30 -> 3
        //     | _ -> 0
        //
        // let changeComponent =
        //     match change with
        //     | x when x > 0m && x >= 40m  -> 10
        //     | x when x > 0m && x >= 30m  -> 8
        //     | x when x > 0m && x >= 20m  -> 3
        //     | x when x > 0m && x > 10m  -> 1
        //     | _ -> 0
        // directionComponent * (ageComponent + changeComponent)
        
        // TODO: add a bump if the max value is current value
        
        let score = (directionComponent |> decimal) * value
        
        System.Math.Round(score, 2)

    let trendScore (industryBreakdowns:list<IndustrySMABreakdown>) =
        let breakdowns = industryBreakdowns |> List.map (fun x -> x.breakdown)
        
        let trendWithCycle = breakdowns |> TrendsCalculator.calculate

        let trend = trendWithCycle.trend

        calculateScore trend.value trend.direction trend.streak trend.change

    let cycleScore (industryBreakdowns:list<IndustrySMABreakdown>) =
        let breakdowns = industryBreakdowns |> List.map (fun x -> x.breakdown)
        
        let trendWithCycle = breakdowns |> TrendsCalculator.calculate

        let trend = trendWithCycle.trend
        let cycle = trendWithCycle.cycle

        calculateScore cycle.currentPoint.value trend.direction cycle.ageInMarketDays cycle.change