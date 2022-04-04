module ReportTests

open Xunit
open Xunit.Abstractions

type ReportTests(output:ITestOutputHelper) =
    do
        FinvizScraper.Storage.Reports.configureConnectionString (System.Environment.GetEnvironmentVariable(StorageTests.dbEnvironmentVariableName))
        FinvizScraper.Storage.Storage.configureConnectionString (System.Environment.GetEnvironmentVariable(StorageTests.dbEnvironmentVariableName))

    let topGroupingTest resultGenerator containsMember =

        let screener = FinvizScraper.Storage.Storage.getScreenerByName StorageTests.testScreenerName
        
        match screener with
            | Some screener ->
                let grouping = screener |> resultGenerator
                let length = grouping |> Seq.length
                Assert.NotEqual(0, length)

                let index = Seq.tryFind (fun (name, _) -> name = containsMember) grouping
                match index with
                    | Some _ ->
                        Assert.True(true)
                    | None ->
                        Assert.True(false, $"{containsMember} not found in {grouping}")

            | None -> Assert.True(false, "Expected screener to be found")

    let parseDate dateString =
        System.DateTime.ParseExact(dateString, "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture)

    [<Theory>]
    [<InlineData("2022-04-01")>]
    let ``Getting sectors works`` dateStr =
        let date = parseDate dateStr
        "Energy" |> topGroupingTest (fun x-> FinvizScraper.Storage.Reports.topSectors x.id date)

    [<Theory>]
    [<InlineData("2022-04-01")>]
    let ``Getting industry works`` dateStr =
        let date = parseDate dateStr
        "Telecom Services" |> topGroupingTest (fun x-> FinvizScraper.Storage.Reports.topIndustries x.id date)

    [<Theory>]
    [<InlineData("2022-04-01")>]
    let ``Getting countries works`` dateStr =
        let date = parseDate dateStr
        "USA" |> topGroupingTest (fun x-> FinvizScraper.Storage.Reports.topCountries x.id date)

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

    [<Fact>]
    let ``Particular screener daily counts work``() =

        let screener = FinvizScraper.Storage.Storage.getScreenerByName StorageTests.testScreenerName

        let results = FinvizScraper.Storage.Reports.getDailyCountsForScreener screener.Value.id 7

        Assert.NotEmpty(results)

        let (_,firstCount) = results.Item(0)
        Assert.True(firstCount > 0)