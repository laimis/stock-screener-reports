module IndustryAlertGeneratorTests

open System
open StockScreenerReports.Core
open Xunit
open FsUnit


type IndustryAlertGeneratorTests() =
    let screener : Screener = {
        id = 1
        name = "name"
        url = "url"
    }
    
    let generateScreenerResultForIndustry industry : ScreenerResultReportItem =
        {
            country = "country"
            industry = industry
            price = 1.0m
            sector = "sector"
            ticker = "amd"
            change = 10m
            volume = 12
            marketCap = 12m
            date = DateTime.UtcNow
            name = "companyname"
            screenerid = 1
            screenername = "screenername"
            stockid = 1 
        }
        
    let generateIndustrySMABreakdown count industry : IndustrySMABreakdown =
        {
            industry = industry
            breakdown = {
                above = 5
                below = count
                date = DateTime.UtcNow
                days = SMA.SMA20 
            } 
        }
        
    let generateMapFromLists count (input:ScreenerResultReportItem list list) =
        input
        |> List.concat
        |> List.distinctBy _.industry
        |> List.map _.industry
        |> List.distinct
        |> List.map (fun x -> (x, count))
        |> Map.ofList
    
    [<Fact>]
    let ``When industry has more than 20 results, it ends up on alerts``() =
        
        let firstList =
            [
                yield! List.init 20 (fun _ -> generateScreenerResultForIndustry "industry1")
                yield! List.init 5 (fun _ -> generateScreenerResultForIndustry "industry2")
                yield! List.init 30 (fun _ -> generateScreenerResultForIndustry "industry3")
            ]
            
        let secondList = List.init 20 (fun _ -> generateScreenerResultForIndustry "industry4")
        
        let screenerMap = [firstList; secondList] |> generateMapFromLists 25
        
        let input = [(screener,firstList); (screener,secondList)]
        
        let result = IndustryAlertGenerator.screenerAlerts screenerMap input
        
        let industries = result |> List.map _.industry
        
        industries |> should contain "industry1"
        industries |> should contain "industry3"
        industries |> should not' (contain "industry2")
        industries |> should contain "industry4"
        
    [<Fact>]
    let ``When industry results are dominating percentage wise, it ends up on alerts``() =
        
        let firstList =
            [
                yield! List.init 5 (fun _ -> generateScreenerResultForIndustry "industry1")
                yield! List.init 3 (fun _ -> generateScreenerResultForIndustry "industry2")
                yield! List.init 3 (fun _ -> generateScreenerResultForIndustry "industry3")
            ]
            
        let secondList =
            [
                yield! List.init 1 (fun _ -> generateScreenerResultForIndustry "industry4")
                yield! List.init 1 (fun _ -> generateScreenerResultForIndustry "industry5")
                yield! List.init 1 (fun _ -> generateScreenerResultForIndustry "industry6")
                yield! List.init 1 (fun _ -> generateScreenerResultForIndustry "industry7")
            ]
        
        let input = [(screener,firstList); (screener,secondList)]
        
        let screenerMap = [firstList; secondList] |> generateMapFromLists 10
        
        let result = IndustryAlertGenerator.screenerAlerts screenerMap input
        
        result.Length |> should equal 1
        result.Head.industry |> should equal "industry1"
        
    [<Fact>]
    let ``When industry results are dominating industry breakdown, it ends up on alerts``() =
        
        let list = [
            yield! List.init 5 (fun _ -> generateScreenerResultForIndustry "industry1")
            yield! List.init 5 (fun _ -> generateScreenerResultForIndustry "industry2")
            yield! List.init 5 (fun _ -> generateScreenerResultForIndustry "industry3")
            yield! List.init 5 (fun _ -> generateScreenerResultForIndustry "industry4")
        ]
        
        let screenerMap = [list] |> generateMapFromLists 25 |> Map.add "industry1" 5
        
        let input = [(screener,list)]
        
        let result = IndustryAlertGenerator.screenerAlerts screenerMap input
        
        result.Length |> should equal 1
        result |> List.map _.industry |> should contain "industry1"
        
    [<Fact>]
    let ``When result count is small, no alerts are generated``() =
        
        let list = [
            yield! List.init 1 (fun _ -> generateScreenerResultForIndustry "industry1")
        ]
        
        let screenerMap = [list] |> generateMapFromLists 25
        
        let input = [(screener,list)]
        
        let result = IndustryAlertGenerator.screenerAlerts screenerMap input
        
        result.Length |> should equal 0
        
    [<Fact>]
    let ``When industry SMA breakdown has 90% or above, it generates an alert``() =
        
        let meetsThreshold = {
            industry = "Sample"
            trend = {
                change = 0m
                direction = Up
                streak = 0
                value = 0m 
            }
            above = 9
            below = 1
            date = DateTime.UtcNow
            days = SMA20 
        }
        
        let failsThreshold = {
            industry = "Sample 2"
            trend = {
                change = 0m
                direction = Up
                streak = 0
                value = 0m 
            }
            above = 9
            below = 2
            date = DateTime.UtcNow
            days = SMA20 
        }
        
        let result = IndustryAlertGenerator.industryTrendAlerts [meetsThreshold; failsThreshold]
        
        result.Length |> should equal 1