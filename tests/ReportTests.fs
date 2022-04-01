module ReportTests

open Xunit
open Xunit.Abstractions

type ReportTests(output:ITestOutputHelper) =
    do
        FinvizScraper.Storage.Reports.configureConnectionString (System.Environment.GetEnvironmentVariable(StorageTests.dbEnvironmentVariableName))
        FinvizScraper.Storage.Storage.configureConnectionString (System.Environment.GetEnvironmentVariable(StorageTests.dbEnvironmentVariableName))

    let topGroupingTest resultGenerator =
        let screener = FinvizScraper.Storage.Storage.getScreenerByName StorageTests.testScreenerName
        
        match screener with
            | Some screener ->
                let length =
                    screener
                    |> resultGenerator
                    |> Seq.length
                Assert.NotEqual(0, length)

            | None -> Assert.True(false, "Expected screener to be found")

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(7)>]
    let ``Getting sectors works`` days =

        topGroupingTest (fun x-> FinvizScraper.Storage.Reports.topSectors x days)

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(7)>]
    let ``Getting industry works`` days =
        topGroupingTest (fun x-> FinvizScraper.Storage.Reports.topIndustries x days)

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(7)>]
    let ``Getting countries works`` days =

        topGroupingTest (fun x-> FinvizScraper.Storage.Reports.topCountries x days)

    [<Fact>]
    let ``Screener result list works``() =
        
        let expectedScreenerCount = 
            FinvizScraper.Storage.Storage.getScreeners()
            |> List.filter (fun x -> x.name.StartsWith("screener") |> not)
            |> Seq.length

        let screenerResults = FinvizScraper.Storage.Reports.getLatestScreeners()

        Assert.Equal(expectedScreenerCount, screenerResults.Length)

    [<Fact>]
    let ``Particular screener results list works``() =

        let screener = FinvizScraper.Storage.Reports.getLatestScreeners().Head

        let results = FinvizScraper.Storage.Reports.getScreenerResults screener.screenerid (screener.date.ToString("yyyy-MM-dd"))

        Assert.NotEmpty(results)