module tests.TrendCalculationTests

open StockScreenerReports.Core
open StockScreenerReports.Storage
open StorageTests
open Xunit
open FsUnit
open Xunit.Abstractions

type TrendCalculationTests(output:ITestOutputHelper) =
    // output:ITestOutputHelper - add this above if you need output
    do
        Reports.configureConnectionString (SecretsHelper.getSecret(dbEnvironmentVariableName))
        Storage.configureConnectionString (SecretsHelper.getSecret(dbEnvironmentVariableName))


    [<Fact>]
    let ``Screener result list works``() =
        
        let describeDurations (durations:int list) =
            let count = durations.Length

            let averageDuration = if count > 0 then (durations |> List.sum |> decimal) / (decimal count) else 0m
            let medianDuration = if count > 0 then durations |> List.sort |> (fun l -> l[l.Length / 2]) else 0
            let maxDuration = if count > 0 then durations |> List.max else 0
            let minDuration = if count > 0 then durations |> List.min else 0

            $"Number of occurrences: %d{count}" |> output.WriteLine
            $"Average duration: %.2f{averageDuration}" |> output.WriteLine
            $"Median duration: %d{medianDuration}" |> output.WriteLine
            $"Maximum duration: %d{maxDuration}" |> output.WriteLine
            $"Minimum duration: %d{minDuration}" |> output.WriteLine
            
        let analyzeSequence (values: decimal list) =
            let folder (prevDuration, durations) current =
                if current >= 90m then
                    let newDuration = prevDuration + 1
                    (newDuration, durations)
                else
                    let updatedDurations = if prevDuration > 0 then prevDuration :: durations else durations
                    (0, updatedDurations)

            let initialState = (0, [])
            let lastDuration, durations = List.fold folder initialState values

            if lastDuration > 0 then lastDuration :: durations else durations
            
        let values = [64.29m; 50.00m; 54.76m; 54.76m; 57.14m; 66.67m; 80.01m; 93.01m; 99.01m; 100.00m; 92.00m; 91.00m; 88.00m; 33.33m; 52.38m; 52.38m; 40.48m; 38.10m; 66.67m; 80.01m; 93.01m; 99.01m; 91.00m; 88.00m]
        let durations = analyzeSequence values
        describeDurations durations
        
        // get all the industries
        let industries = Storage.getIndustries()
        
        let realDurations =
            industries
            |> List.sort
            |> List.map(
                fun industry ->
                    
                    let stats =
                        industry
                        |> Reports.getAllIndustrySMABreakdowns SMA.SMA20
                        |> List.map _.breakdown.percentAbove 
                        |> analyzeSequence
                        
                    output.WriteLine($"Industry: {industry}")
                    stats |> describeDurations
                    stats
                    
            )
            |> List.concat
            
        realDurations |> describeDurations