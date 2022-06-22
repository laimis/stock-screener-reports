module FinvizClientTests

open Xunit
open FinvizScraper.FinvizClient
open Xunit.Abstractions

type ParsingTests(output:ITestOutputHelper) =

    // all time high screener
    [<Fact>]
    let ``End to end fetch works`` () =
        let results =
            StorageTests.screenerUrl
            |> FinvizClient.getResults

        Assert.NotEmpty(results)

    [<Fact>]
    let ``Fetch count works`` () =
        let count = 
            StorageTests.screenerUrl
            |> FinvizClient.getResultCount

        Assert.True(count > 0, "Result count for test screener should be greater than 0")

    [<Fact>]
    let ``industry fetch works`` () =
        let (above,below) =
            StorageTests.testStockIndustry
            |> FinvizClient.getResultCountForIndustryAboveAndBelowSMA 20 

        Assert.True(above > 0)
        Assert.True(below > 0)


    [<Fact>]
    let ``industry with special characters works`` () =
        let (above,below) =
            StorageTests.testStockIndustryWithSpecialCharacters
            |> FinvizClient.getResultCountForIndustryAboveAndBelowSMA 20 

        let total = above + below

        Assert.True(total < 100)