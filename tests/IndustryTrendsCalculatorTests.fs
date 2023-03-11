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
                    date = DateTime.UtcNow.AddDays(index);
                    days = 200;
                }
            }
        )
    let testDataDecreasingTrend =
        [(1, 10); (8, 10); (7, 10)]
        |> generateBreakdowns

    let testDataIncreasingTrend =
        [(6, 10); (7, 10); (8, 10)]
        |> generateBreakdowns

    let trendFromZero =
        [(0, 10); (0, 10); (0, 10); (7, 10); (8, 10)]
        |> generateBreakdowns

    let sampleCopperTrend =
        [(0, 4);(0, 4);(1, 4);(2, 4);(2, 4);(1, 4);(1, 5);(1, 5);(1, 5);(1, 5);(1, 5);(2, 5);(5, 5);(5, 5);(5, 5);(5, 5);(1, 5);(2, 5);(0, 5);(0, 5);]
        |> generateBreakdowns

    let sampleResidentialConstructionTrend =
        [(22, 22);(22, 22);(22, 22);(22, 22);(22, 22);(20, 22);(20, 22);(19, 22);(21, 22);(21, 22);(19, 22);(15, 22);(7, 22);(3, 23);(3, 23);(6, 23);(4, 23);(7, 23);(9, 23);(9, 23);(10, 23);(13, 23);(9, 23);(8, 23);(14, 23);(10, 23);(3, 23);]
        |> generateBreakdowns

    let sampleLumberProduction =
        [(3, 5);(3, 5);(3, 5);(2, 5);(2, 5);(3, 5);(4, 5);(4, 5);(4, 5);(3, 5);(3, 5);(3, 5);(3, 5);(3, 5);(4, 5);(4, 5);(3, 5);(3, 5);(3, 5);(4, 5);(5, 5);(4, 5);(3, 5);(1, 5);(1, 5);(1, 5);(1, 5);(1, 5);(0, 5);(0, 5);(0, 5);(0, 5);(2, 5);(1, 5);(0, 5);(1, 5);(0, 5);(1, 5);]
        |> generateBreakdowns

    [<Fact>]
    let ``trending down works`` () =
            
        let trend = TrendsCalculator.calculateForIndustry testDataDecreasingTrend
        
        Assert.Equal(1, trend.streak)
        Assert.Equal(Down, trend.direction)
        Assert.Equal(-10m, System.Math.Round(trend.change, 2))

    [<Fact>]
    let ``trending up works`` () =
            
        let trend = TrendsCalculator.calculateForIndustry testDataIncreasingTrend
        
        Assert.Equal(2, trend.streak)
        Assert.Equal(Up, trend.direction)
        Assert.Equal(20m, System.Math.Round(trend.change, 2))

    [<Fact>]
    let ``encountering zero should stop``() =
        let trend = TrendsCalculator.calculateForIndustry trendFromZero

        Assert.Equal(2, trend.streak)
        Assert.Equal(Up, trend.direction)
        Assert.Equal(80m, System.Math.Round(trend.change, 2))

    [<Fact>]
    let ``copper trend works``() =
        let trend = TrendsCalculator.calculateForIndustry sampleCopperTrend

        Assert.Equal(2, trend.streak)
        Assert.Equal(Down, trend.direction)
        Assert.Equal(-40m, System.Math.Round(trend.change, 2))

    [<Fact>]
    let ``residential construction trend works``() =
        let trend = TrendsCalculator.calculateForIndustry sampleResidentialConstructionTrend

        Assert.Equal(2, trend.streak)
        Assert.Equal(Down, trend.direction)
        Assert.Equal(-47.83m, System.Math.Round(trend.change, 2))

    [<Fact>]
    let ``lumber production trend works``() =
        let trend = TrendsCalculator.calculateForIndustry sampleLumberProduction

        Assert.Equal(3, trend.streak)
        Assert.Equal(Up, trend.direction)
        Assert.Equal(20m, System.Math.Round(trend.change, 2))