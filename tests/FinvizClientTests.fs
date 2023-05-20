module FinvizClientTests

open Xunit
open StockScreenerReports.FinvizClient
open Xunit.Abstractions

type ParsingTests(output:ITestOutputHelper) =
    do
        FinvizClient.setOutputFunc (fun str -> output.WriteLine(str))

    let screenerUrl = "https://finviz.com/screener.ashx?v=111&f=cap_mega,sh_price_50to100&ft=4" // megastocks with price between 50 and 100

    // all time high screener
    [<Fact>]
    let ``End to end fetch works`` () =
        let results =
            screenerUrl
            |> FinvizClient.getResults

        Assert.NotEmpty(results)

    [<Fact>]
    let ``End to end earnings works`` () =
        let earnings = FinvizClient.getEarnings()

        Assert.NotEmpty(earnings)

    [<Fact>]
    let ``Fetch count works`` () =
        let count = 
            screenerUrl
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