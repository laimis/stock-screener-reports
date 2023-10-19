namespace StockScreenerReports.Core

module MarketCycleScoring =
    
    // NOTE: this is a very simple scoring system, I keep on tweaking this and not
    // happy with what it gives back. Right now the best way to judge the industry
    // is to look at the SMA trend chart and use manual visual inspection.
    // This helps a bit to filter industries but still top result might not be top
    // industry.
    let private cycleScoreInternal cycle trend =
        
        let score =
            cycle.currentPoint.value
            |> fun x ->
                // if trend is negative, subtract trend value from the cycle value
                match trend.direction with
                | Down -> x - trend.change
                | Up -> x
            |> fun x ->
                // if the current point is the max point, add 10 pt bump
                match cycle.highPoint.value = cycle.currentPoint.value with
                | true -> x + 10m
                | false -> x
            
        System.Math.Round(score, 2)
        
    let private trendScoreInternal (trend:Trend) =
        
        System.Math.Round(trend.change, 2)

    let trendScore (industryBreakdowns:list<IndustrySMABreakdown>) =
        let breakdowns = industryBreakdowns |> List.map (fun x -> x.breakdown)
        
        let trendWithCycle = breakdowns |> TrendsCalculator.calculate

        let trend = trendWithCycle.trend

        trendScoreInternal trend

    let cycleScore (industryBreakdowns:list<IndustrySMABreakdown>) =
        let breakdowns = industryBreakdowns |> List.map (fun x -> x.breakdown)
        
        let trendWithCycle = breakdowns |> TrendsCalculator.calculate

        let trend = trendWithCycle.trend
        let cycle = trendWithCycle.cycle

        cycleScoreInternal cycle trend