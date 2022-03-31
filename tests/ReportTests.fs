module ReportTests

open Xunit

type ReportTests() =
    do
        FinvizScraper.Reports.configureConnectionString (System.Environment.GetEnvironmentVariable(StorageTests.dbEnvironmentVariableName))
        FinvizScraper.Storage.configureConnectionString (System.Environment.GetEnvironmentVariable(StorageTests.dbEnvironmentVariableName))

    let topGroupingTest resultGenerator =
        let screener = FinvizScraper.Storage.getScreenerByName StorageTests.testScreenerName
        
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

        topGroupingTest (fun x-> FinvizScraper.Reports.topSectors x days)

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(7)>]
    let ``Getting industry works`` days =
        topGroupingTest (fun x-> FinvizScraper.Reports.topIndustries x days)

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(7)>]
    let ``Getting countries works`` days =

        topGroupingTest (fun x-> FinvizScraper.Reports.topCountries x days)