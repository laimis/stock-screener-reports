module ReportTests

open Xunit
open FinvizScraper.Reports

type ReportTests() =
    do
        storeCnn StorageTests.testConnectionString

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

        topGroupingTest (fun x-> topSectors x days)

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(7)>]
    let ``Getting industry works`` days =
        topGroupingTest (fun x-> topIndustries x days)

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(7)>]
    let ``Getting countries works`` days =

        topGroupingTest (fun x-> topCountries x days)