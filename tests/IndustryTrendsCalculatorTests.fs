module IndustryTrendsCalculatorTests

open Xunit.Abstractions
open Xunit
open FinvizScraper.Core
open System

type IndustryTrendsCalculatorTests(output:ITestOutputHelper) =

    [<Fact>]
    let ``trending up works`` () =
            
            let smaBreakdowns =
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
            
            let (streak, direction, change) = IndustryTrendsCalculator.calculate smaBreakdowns
            
            Assert.Equal(1, streak)
            Assert.Equal(Down, direction)
            Assert.Equal(-40m, change)