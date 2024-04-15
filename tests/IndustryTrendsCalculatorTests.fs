module IndustryTrendsCalculatorTests

open Xunit.Abstractions
open Xunit
open StockScreenerReports.Core
open System
open FsUnit

type IndustryTrendsCalculatorTests(output:ITestOutputHelper) =

    let generateBreakdowns list =
        list
        |> List.indexed
        |> List.map (fun (index,pairs) ->

            let above,total = pairs
            {
                industry = "technology"
                breakdown = {
                    above = above;
                    below = total - above;
                    date = ReportsConfig.now().AddDays(-list.Length).AddDays(index);
                    days = SMA20;
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
            
        let trendWithCycle = TrendsCalculator.calculateForIndustry testDataDecreasingTrend

        let trend = trendWithCycle.trend

        trend.streak |> should equal 1
        trend.direction |> should equal Down
        Math.Round(trend.change, 2) |> should equal -10m

        let cycle = trendWithCycle.cycle

        cycle.age.TotalDays |> should (equalWithin 0.01) 2.0
        cycle.highPointAge.TotalDays |> should (equalWithin 0.01) 1.0
        cycle.highPointValue |> should equal 80.0m
        cycle.startPointValue |> should equal 10.0m

    let testDataIncreasingTrend =
        [(6, 10); (7, 10); (8, 10)]
        |> generateBreakdowns

    [<Fact>]
    let ``trending up works`` () =
            
        let trendWithCycle = TrendsCalculator.calculateForIndustry testDataIncreasingTrend
        let trend = trendWithCycle.trend
        trend.streak |> should equal 2
        trend.direction |> should equal Up
        Math.Round(trend.change, 2) |> should equal 20m

        let cycle = trendWithCycle.cycle

        cycle.age.TotalDays |> should (equalWithin 0.01) 2.0
        cycle.highPointAge.TotalDays |> should (equalWithin 0.01) 0.0
        cycle.highPointValue |> should equal 80.0m
        cycle.startPointValue |> should equal 60.0m

    let trendFromZero =
        [(0, 10); (0, 10); (0, 10); (7, 10); (8, 10)]
        |> generateBreakdowns

    [<Fact>]
    let ``encountering zero should stop``() =
        let trendWithCycle = TrendsCalculator.calculateForIndustry trendFromZero

        let trend = trendWithCycle.trend
        trend.streak |> should equal 2
        trend.direction |> should equal Up
        Math.Round(trend.change, 2) |> should equal 80m
        
        let cycle = trendWithCycle.cycle
        cycle.age.TotalDays |> should (equalWithin 0.01) 2.0
        cycle.highPointAge.TotalDays |> should (equalWithin 0.01) 0.0
        cycle.highPointValue |> should equal 80.0m
        cycle.startPointValue |> should equal 0.0m

    let sampleCopperTrend =
        [(0, 4);(0, 4);(1, 4);(2, 4);(2, 4);(1, 4);(1, 5);(1, 5);(1, 5);(1, 5);(1, 5);(2, 5);(5, 5);(5, 5);(5, 5);(5, 5);(1, 5);(2, 5);(0, 5);(0, 5);]
        |> generateBreakdowns

    [<Fact>]
    let ``copper trend works``() =
        let trendWithCycle = TrendsCalculator.calculateForIndustry sampleCopperTrend

        let trend = trendWithCycle.trend
        trend.streak |> should equal 2
        trend.direction |> should equal Down
        Math.Round(trend.change, 2) |> should equal -40m

    [<Fact>]
    let ``test simple trends``() =
        let runTest data streak change direction =
            let trendWithCycle = data |> generateBreakdowns |> TrendsCalculator.calculateForIndustry
            let trend = trendWithCycle.trend

            trend.streak |> should equal streak
            trend.change |> should equal change
            trend.direction |> should equal direction

        runTest [(4, 10); (2, 10); (0, 10)] 2 -40m Down
        runTest [(4, 10); (0, 10); (0, 10)] 2 -40m Down
        runTest [(4, 10); (6, 10); (8, 10); ] 2 40m Up
        runTest [(4, 10); (6, 10); (8, 10); (6,10)] 1 -20m Down
        

    [<Fact>]
    let ``simple trend of zeros`` () =
        let data = [(0, 6); (0, 6)]
        let trendWithCycle = data |> generateBreakdowns |> TrendsCalculator.calculateForIndustry

        let trend = trendWithCycle.trend
        trend.streak |> should equal 1
        trend.change |> should equal 0m
        trend.direction |> should equal Up

        let cycle = trendWithCycle.cycle
        cycle.age.TotalDays |> should (equalWithin 0.01) 0.0
        cycle.highPointAge.TotalDays |> should (equalWithin 0.01) 0.0
        cycle.highPointValue |> should equal 0.0m
        cycle.startPointValue |> should equal 0.0m

    let sampleResidentialConstructionTrend =
        [(22, 22);(22, 22);(22, 22);(22, 22);(22, 22);(20, 22);(20, 22);(19, 22);(21, 22);(21, 22);(19, 22);(15, 22);(7, 22);(3, 23);(3, 23);(6, 23);(4, 23);(7, 23);(9, 23);(9, 23);(10, 23);(13, 23);(9, 23);(8, 23);(14, 23);(10, 23);(3, 23);]
        |> generateBreakdowns

    [<Fact>]
    let ``residential construction trend works``() =
        let trendWithCycle = TrendsCalculator.calculateForIndustry sampleResidentialConstructionTrend

        let trend = trendWithCycle.trend
        trend.streak |> should equal 2
        trend.direction |> should equal Down
        Math.Round(trend.change, 2) |> should equal -47.83m

        let cycle = trendWithCycle.cycle
        cycle.age.TotalDays |> should (equalWithin 0.01) 0
        cycle.highPointAge.TotalDays |> should (equalWithin 0.01) 0
        Math.Round(cycle.highPointValue,0) |> should equal 13m
        cycle.startPointValue |> should (equalWithin 0.02) 13.04m

    [<Fact>]
    let ``lumber production trend works``() =
        let trendWithCycle = TrendsCalculator.calculateForIndustry sampleLumberProduction
        let trend = trendWithCycle.trend
        trend.streak |> should equal 1
        trend.direction |> should equal Up
        Math.Round(trend.change, 2) |> should equal 20m
        
        
    [<Fact>]
    let ``industry sequence detection works`` () =
        let sequences = TrendsCalculator.calculateSequences sampleResidentialConstructionTrend
        
        sequences |> should haveLength 2
        
        let underTest = sequences |> List.last
        
        underTest.length |> should equal 7
        underTest.start.value |> should be (greaterThanOrEqualTo 90)
        underTest.start.date.Date |> should equal (DateTime.Parse("2024-03-19"))
        underTest.end'.value |> should be (greaterThanOrEqualTo 90)
        underTest.end'.date.Date |> should equal (DateTime.Parse("2024-03-25"))
        underTest.age.TotalDays |> int |> should equal 6
        underTest.length |> should equal 7
        underTest.open' |> should be False
        underTest.values |> List.forall (fun x -> x.value >= 90m) |> should be True
        