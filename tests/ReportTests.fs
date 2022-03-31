module ReportTests

open Xunit

type ReportTests() =
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