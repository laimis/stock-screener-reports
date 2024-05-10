module FinvizClientTests

open System
open Xunit
open StockScreenerReports.Core
open StockScreenerReports.FinvizClient
open Xunit.Abstractions
open FsUnit

type ParsingTests(output: ITestOutputHelper) =
    let getOutput () = output

    do
        output.WriteLine("Running FinvizClientTests")
        FinvizClient.setOutputFunc (fun str -> output.WriteLine(str))

    let screenerUrl =
        "https://finviz.com/screener.ashx?v=111&f=cap_mega,sh_price_50to100&ft=4" // megastocks with price between 50 and 100

    // all time high screener
    [<Fact>]
    let ``End to end fetch works`` () =
        let results = screenerUrl |> FinvizClient.getResults

        results |> should not' (be Empty)

    [<Fact>]
    let ``End to end earnings works`` () =
        let earnings = FinvizClient.getEarnings ()

        earnings |> should not' (be Empty)

    [<Fact>]
    let ``Fetch count works`` () =
        let count = screenerUrl |> FinvizClient.getResultCount

        count |> should be (greaterThan 0)

    [<Fact>]
    let ``industry fetch works`` () =
        let (above, below) =
            StorageTests.testStockIndustry
            |> FinvizClient.getResultCountForIndustryAboveAndBelowSMA SMA20

        above |> should be (greaterThan 0)
        below |> should be (greaterThan 0)


    [<Fact>]
    let ``industry with special characters works`` () =
        let (above, below) =
            StorageTests.testStockIndustryWithSpecialCharacters
            |> FinvizClient.getResultCountForIndustryAboveAndBelowSMA SMA20

        let total = above + below

        total |> should be (lessThan 100)


    [<Fact>]
    let ``country fetch works`` () =
        let (above, below) =
            StorageTests.testStockCountry
            |> FinvizClient.getResultCountForCountryAboveAndBelowSMA SMA20

        above |> should be (greaterThan 0)
        below |> should be (greaterThan 0)

    with
        [<Fact>]
        member this.CorporateActions() =

            let content = StockAnalysisClient.getCorporateActions ()

            content |> should not' (be Empty)

            let sccoAction =
                content
                |> List.find (fun (act) -> act.Symbol = "SCCO")

            sccoAction.Date |> should be (equal "May 7, 2024")
            sccoAction.Date |> DateTime.Parse |> _.Month |> should be (equal 5)
            sccoAction.Symbol |> should be (equal "SCCO")
            sccoAction.Type |> should be (equal "Stock Split")
            sccoAction.Action |> should be (equal "SCCO stock split: 1.0104 for 1")