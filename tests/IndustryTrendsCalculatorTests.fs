module IndustryTrendsCalculatorTests

open Xunit.Abstractions
open Xunit
open FinvizScraper.Core
open System

type IndustryTrendsCalculatorTests(output:ITestOutputHelper) =

    let testDataDecreasingTrend =
        [
            {
                industry = "Technology"
                breakdown = {
                    above = 1
                    below = 14
                    date = DateTime.Parse("1/1/2019")
                    days = 20
                }
            };
            {
                industry = "Technology"
                breakdown = {
                    above = 8
                    below = 7
                    date = DateTime.Parse("1/2/2019")
                    days = 20
                }
            };
            {
                industry = "Technology"
                breakdown = {
                    above = 7
                    below = 8
                    date = DateTime.Parse("1/3/2019")
                    days = 20
                }
            };
        ]

    let testDataIncreasingTrend =
        [
            {
                industry = "Technology"
                breakdown = {
                    above = 6
                    below = 9
                    date = DateTime.Parse("1/1/2019")
                    days = 20
                }
            };
            {
                industry = "Technology"
                breakdown = {
                    above = 7
                    below = 8
                    date = DateTime.Parse("1/2/2019")
                    days = 20
                }
            };
            {
                industry = "Technology"
                breakdown = {
                    above = 8
                    below = 7
                    date = DateTime.Parse("1/3/2019")
                    days = 20
                }
            };
        ]


    [<Fact>]
    let ``trending down works`` () =
            
            let (streak, direction, change) = IndustryTrendsCalculator.calculate testDataDecreasingTrend
            
            Assert.Equal(1, streak)
            Assert.Equal(Down, direction)
            Assert.Equal(-6.67m, System.Math.Round(change, 2))

    [<Fact>]
    let ``trending up works`` () =
            
            let (streak, direction, change) = IndustryTrendsCalculator.calculate testDataIncreasingTrend
            
            Assert.Equal(2, streak)
            Assert.Equal(Up, direction)
            Assert.Equal(13.33m, System.Math.Round(change, 2))