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


    [<Fact>]
    let ``trending down works`` () =
            
        let (streak, direction, change) = IndustryTrendsCalculator.calculateForIndustry testDataDecreasingTrend
        
        Assert.Equal(1, streak)
        Assert.Equal(Down, direction)
        Assert.Equal(-10m, System.Math.Round(change, 2))

    [<Fact>]
    let ``trending up works`` () =
            
        let (streak, direction, change) = IndustryTrendsCalculator.calculateForIndustry testDataIncreasingTrend
        
        Assert.Equal(2, streak)
        Assert.Equal(Up, direction)
        Assert.Equal(20m, System.Math.Round(change, 2))

    [<Fact>]
    let ``encountering zero should stop``() =
        let (streak, direction, change) = IndustryTrendsCalculator.calculateForIndustry trendFromZero

        Assert.Equal(2, streak)
        Assert.Equal(Up, direction)
        Assert.Equal(80m, System.Math.Round(change, 2))
