module FinvizClientTests

open Xunit
open StockScreenerReports.FinvizClient
open Xunit.Abstractions
open FsUnit

type ParsingTests(output:ITestOutputHelper) =
    do
        output.WriteLine("Running FinvizClientTests")
        FinvizClient.setOutputFunc (fun str -> output.WriteLine(str))

    let screenerUrl = "https://finviz.com/screener.ashx?v=111&f=cap_mega,sh_price_50to100&ft=4" // megastocks with price between 50 and 100

    // all time high screener
    [<Fact>]
    let ``End to end fetch works`` () =
        let results =
            screenerUrl
            |> FinvizClient.getResults

        results |> should not' (be Empty)

    [<Fact>]
    let ``End to end earnings works`` () =
        let earnings = FinvizClient.getEarnings()

        earnings |> should not' (be Empty)

    [<Fact>]
    let ``Fetch count works`` () =
        let count = 
            screenerUrl
            |> FinvizClient.getResultCount

        count |> should be (greaterThan 0)

    [<Fact>]
    let ``industry fetch works`` () =
        let (above,below) =
            StorageTests.testStockIndustry
            |> FinvizClient.getResultCountForIndustryAboveAndBelowSMA 20 

        above |> should be (greaterThan 0)
        below |> should be (greaterThan 0)


    [<Fact>]
    let ``industry with special characters works`` () =
        let (above,below) =
            StorageTests.testStockIndustryWithSpecialCharacters
            |> FinvizClient.getResultCountForIndustryAboveAndBelowSMA 20 

        let total = above + below

        total |> should be (lessThan 100)
        
        
    [<Fact>]
    let ``country fetch works`` () =
        let (above,below) =
            StorageTests.testStockCountry
            |> FinvizClient.getResultCountForCountryAboveAndBelowSMA 20 

        above |> should be (greaterThan 0)
        below |> should be (greaterThan 0)