module IndustryTrendsCalculatorTests

open Xunit.Abstractions
open Xunit
open StockScreenerReports.Core
open System

type IndustryTrendsCalculatorTests(output:ITestOutputHelper) =

    let generateBreakdowns list =
        list
        |> List.indexed
        |> List.map (fun (index,pairs) ->

            let (above,total) = pairs
            {
                industry = "technology"
                breakdown = {
                    above = above;
                    below = total - above;
                    date = DateTime.UtcNow.AddDays(-list.Length).AddDays(index);
                    days = Constants.SMA20;
                }
            }
        )

    let sampleLumberProduction =
        [(3, 5);(3, 5);(3, 5);(2, 5);(2, 5);(3, 5);(4, 5);(4, 5);(4, 5);(3, 5);(3, 5);(3, 5);(3, 5);(3, 5);(4, 5);(4, 5);(3, 5);(3, 5);(3, 5);(4, 5);(5, 5);(4, 5);(3, 5);(1, 5);(1, 5);(1, 5);(1, 5);(1, 5);(0, 5);(0, 5);(0, 5);(0, 5);(2, 5);(1, 5);(0, 5);(1, 5);(0, 5);(1, 5);]
        |> generateBreakdowns

    let testDataDecreasingTrend =
        [(1, 10); (8, 10); (7, 10)]
        |> generateBreakdowns

    [<Fact>]
    let ``trending down works`` () =
            
        let trendWithCycle = TrendsCalculator.calculateTrendAndCycleForIndustry testDataDecreasingTrend

        let trend = trendWithCycle.trend
        
        Assert.Equal(1, trend.streak)
        Assert.Equal(Down, trend.direction)
        Assert.Equal(-10m, System.Math.Round(trend.change, 2))

        let cycle = trendWithCycle.cycle

        Assert.Equal(2.0, cycle.age.TotalDays, 0.01)
        Assert.Equal(1.0, cycle.highPointAge.TotalDays, 0.01)
        Assert.Equal(80.0m, cycle.highPointValue)
        Assert.Equal(10.0m, cycle.lowPointValue)


    let testDataIncreasingTrend =
        [(6, 10); (7, 10); (8, 10)]
        |> generateBreakdowns

    [<Fact>]
    let ``trending up works`` () =
            
        let trendWithCycle = TrendsCalculator.calculateTrendAndCycleForIndustry testDataIncreasingTrend
        let trend = trendWithCycle.trend
        Assert.Equal(2, trend.streak)
        Assert.Equal(Up, trend.direction)
        Assert.Equal(20m, System.Math.Round(trend.change, 2))

        let cycle = trendWithCycle.cycle

        Assert.Equal(2.0, cycle.age.TotalDays, 0.01)
        Assert.Equal(0.0, cycle.highPointAge.TotalDays, 0.01)
        Assert.Equal(80.0m, cycle.highPointValue)
        Assert.Equal(60.0m, cycle.lowPointValue)

    let trendFromZero =
        [(0, 10); (0, 10); (0, 10); (7, 10); (8, 10)]
        |> generateBreakdowns

    [<Fact>]
    let ``encountering zero should stop``() =
        let trendWithCycle = TrendsCalculator.calculateTrendAndCycleForIndustry trendFromZero

        let trend = trendWithCycle.trend
        Assert.Equal(2, trend.streak)
        Assert.Equal(Up, trend.direction)
        Assert.Equal(80m, System.Math.Round(trend.change, 2))

        let cycle = trendWithCycle.cycle
        Assert.Equal(2.0, cycle.age.TotalDays, 0.01)
        Assert.Equal(0.0, cycle.highPointAge.TotalDays, 0.01)
        Assert.Equal(80.0m, cycle.highPointValue)
        Assert.Equal(0.0m, cycle.lowPointValue)

    let sampleCopperTrend =
        [(0, 4);(0, 4);(1, 4);(2, 4);(2, 4);(1, 4);(1, 5);(1, 5);(1, 5);(1, 5);(1, 5);(2, 5);(5, 5);(5, 5);(5, 5);(5, 5);(1, 5);(2, 5);(0, 5);(0, 5);]
        |> generateBreakdowns

    [<Fact>]
    let ``copper trend works``() =
        let trendWithCycle = TrendsCalculator.calculateTrendAndCycleForIndustry sampleCopperTrend

        let trend = trendWithCycle.trend
        Assert.Equal(2, trend.streak)
        Assert.Equal(Down, trend.direction)
        Assert.Equal(-40m, System.Math.Round(trend.change, 2))

        // TODO: will need a pass that adjusts high point age to be the same as the cycle age
        // let cycle = trendWithCycle.cycle
        // Assert.Equal(0.0, cycle.age.TotalDays, 0.01)
        // Assert.Equal(0.0, cycle.highPointAge.TotalDays, 0.01)
        // Assert.Equal(0.0m, cycle.highPointValue)
        // Assert.Equal(0.0m, cycle.lowPointValue)


    [<Fact>]
    let ``test simple trends``() =
        let runTest data streak change direction =
            let trend = data |> generateBreakdowns |> TrendsCalculator.calculateForIndustry

            Assert.Equal(streak, trend.streak)
            Assert.Equal(change, trend.change)
            Assert.Equal(direction, trend.direction)

        runTest [(4, 10); (2, 10); (0, 10)] 2 -40m Down
        runTest [(4, 10); (0, 10); (0, 10)] 2 -40m Down
        runTest [(4, 10); (6, 10); (8, 10); ] 2 40m Up
        runTest [(4, 10); (6, 10); (8, 10); (6,10)] 1 -20m Down
        

    [<Fact>]
    let ``simple trend of zeros`` () =
        let data = [(0, 6); (0, 6)]
        let trendWithCycle = data |> generateBreakdowns |> TrendsCalculator.calculateTrendAndCycleForIndustry

        let trend = trendWithCycle.trend
        Assert.Equal(1, trend.streak)
        Assert.Equal(0m, trend.change)
        Assert.Equal(Up, trend.direction)

        let cycle = trendWithCycle.cycle
        Assert.Equal(0.0, cycle.age.TotalDays, 0.01)
        Assert.Equal(0.0, cycle.highPointAge.TotalDays, 0.01)
        Assert.Equal(0.0m, cycle.highPointValue)
        Assert.Equal(0.0m, cycle.lowPointValue)

    let sampleResidentialConstructionTrend =
        [(22, 22);(22, 22);(22, 22);(22, 22);(22, 22);(20, 22);(20, 22);(19, 22);(21, 22);(21, 22);(19, 22);(15, 22);(7, 22);(3, 23);(3, 23);(6, 23);(4, 23);(7, 23);(9, 23);(9, 23);(10, 23);(13, 23);(9, 23);(8, 23);(14, 23);(10, 23);(3, 23);]
        |> generateBreakdowns

    [<Fact>]
    let ``residential construction trend works``() =
        let trendWithCycle = TrendsCalculator.calculateTrendAndCycleForIndustry sampleResidentialConstructionTrend

        let trend = trendWithCycle.trend
        Assert.Equal(2, trend.streak)
        Assert.Equal(Down, trend.direction)
        Assert.Equal(-47.83m, System.Math.Round(trend.change, 2))

        let cycle = trendWithCycle.cycle
        Assert.Equal(13.0, cycle.age.TotalDays, 0.01)
        Assert.Equal(2.0, cycle.highPointAge.TotalDays, 0.01)
        Assert.Equal(61m, Math.Round(cycle.highPointValue,0))
        Assert.Equal(float 13.04m, float cycle.lowPointValue, 0.02)

    [<Fact>]
    let ``lumber production trend works``() =
        let trend = TrendsCalculator.calculateForIndustry sampleLumberProduction

        Assert.Equal(1, trend.streak)
        Assert.Equal(Up, trend.direction)
        Assert.Equal(20m, System.Math.Round(trend.change, 2))