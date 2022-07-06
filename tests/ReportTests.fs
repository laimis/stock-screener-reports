module ReportTests

open Xunit
open Xunit.Abstractions
open System
open FinvizScraper.Storage

type ReportTests(output:ITestOutputHelper) =
    do
        Reports.configureConnectionString (System.Environment.GetEnvironmentVariable(StorageTests.dbEnvironmentVariableName))
        Storage.configureConnectionString (System.Environment.GetEnvironmentVariable(StorageTests.dbEnvironmentVariableName))

    let getTestScreener = 
        Storage.getScreenerByName StorageTests.testScreenerName

    let getTestSector = "Energy"
    let getTestIndustry = "Agricultural Inputs"
    let getTestCountry = "USA"

    let getTestStartDate() = DateTime.Now.AddDays(-7)
    let getTestEndDate() = DateTime.Now

    let topGroupingTest resultGenerator containsMember =

        let screener = getTestScreener
        
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
        "Energy" |> topGroupingTest (fun x-> Reports.topSectors x.id date)

    [<Theory>]
    [<InlineData("2022-04-01")>]
    let ``Getting industry works`` dateStr =
        let date = parseDate dateStr
        "Telecom Services" |> topGroupingTest (fun x-> Reports.topIndustries x.id date)

    [<Theory>]
    [<InlineData("2022-04-01")>]
    let ``Getting countries works`` dateStr =
        let date = parseDate dateStr
        "USA" |> topGroupingTest (fun x-> Reports.topCountries x.id date)

    [<Fact>]
    let ``Screener result list works``() =
        
        let screenerSet = 
            Storage.getScreeners()
            |> List.map (fun x -> x.id)
            |> Set.ofList

        let screenerResults = Reports.getLatestScreeners()

        let filteredList =
            screenerResults
            |> List.filter (fun x -> screenerSet.Contains(x.screenerid) |> not)

        Assert.Empty(filteredList)

    [<Fact>]
    let ``Particular screener results list works``() =

        let screener = Reports.getLatestScreeners().Head

        let results = Reports.getScreenerResults screener.screenerid (screener.date.ToString("yyyy-MM-dd"))

        Assert.NotEmpty(results)

    [<Fact>]
    let ``Particular screener result for multiple days works``() =

        let screener = Reports.getLatestScreeners().Head
        
        let results = Reports.getScreenerResultsForDays screener.screenerid 14

        Assert.NotEmpty(results)

    [<Fact>]
    let ``Particular screener daily counts work``() =

        let screener = getTestScreener

        let results = Reports.getDailyCountsForScreener screener.Value.id 7

        Assert.NotEmpty(results)

        let (_,firstCount) = results.Item(0)
        Assert.True(firstCount > 0)

    [<Fact>]
    let ``Particular screener daily volume works``() =

        let screener = getTestScreener

        let results = Reports.getDailyAverageVolumeForScreener screener.Value.id 7

        Assert.NotEmpty(results)

        let (_,firstCount) = results.Item(0)
        Assert.True(firstCount > 0)

    [<Fact>]
    let ``Date range sector grouping works``() =
        let start = new DateTime(2022, 6, 1)
        let ending = new DateTime(2022, 6, 30)

        "Energy" |> topGroupingTest (
            fun x -> 
                Reports.topSectorsOverDays x.id start ending
            )

    [<Fact>]
    let ``Date range industry grouping works``() =
        let start = new DateTime(2022, 6, 1)
        let ending = new DateTime(2022, 6, 30)

        "Biotechnology" |> topGroupingTest (
            fun x -> 
                Reports.topIndustriesOverDays x.id start ending
            )

    [<Fact>]
    let ``Date range country grouping works``() =
        "USA" |> topGroupingTest (fun x -> Reports.topCountriesOverDays x.id (DateTime.Now.AddDays(-7)) DateTime.Now)

    [<Fact>]
    let ``getting screener results for ticker works``() =
        let ticker = FinvizScraper.Core.StockTicker.create "cutr"
        let results = Reports.getScreenerResultsForTicker ticker 100
        Assert.NotEmpty(results)

    [<Fact>]
    let ``getting daily counts for screeners filtered by sector works``() =
        let screener = FinvizScraper.Core.Constants.TopGainerScreenerId

        let screenerResult = Reports.getScreenerResultsForDays screener 7

        let sector = screenerResult.Item(0).sector 

        let results = Reports.getDailyCountsForScreenerAndSector screener sector 14
        
        Assert.NotEmpty(results)

    [<Fact>]
    let ``getting daily counts for screeners filtered by industry works``() =
        let screener = getTestScreener
        let industry = getTestIndustry

        let results = Reports.getDailyCountsForScreenerAndIndustry screener.Value.id industry 60
        
        Assert.NotEmpty(results)

    [<Fact>]
    let ``getting daily counts for screeners filtered by country works``() =
        let screener = getTestScreener
        let industry = getTestCountry

        let results = Reports.getDailyCountsForScreenerAndCountry screener.Value.id industry 30
        
        Assert.NotEmpty(results)

    [<Fact>]
    let ``getting trending industries works``() =
        let results = 
            FinvizScraper.Core.Constants.NewHighsScreenerId
            |> Reports.getTopIndustriesForScreener 14

        Assert.NotEmpty(results)

    [<Fact>]
    let ``getting trending sectors works``() =
        let results = 
            FinvizScraper.Core.Constants.NewHighsScreenerId
            |> Reports.getTopSectorsForScreener 14

        Assert.NotEmpty(results)


    [<Fact>]
    let ``getting countries works``() =
        let results = Reports.getStockByCountryBreakdown()

        Assert.NotEmpty(results)

        let country,count = results.Item(0)

        Assert.Equal("USA", country)
        Assert.True(count > 1000)
        Assert.True(count < 8000)

    [<Fact>]
    let ``industry sma breakdowns end to end works`` () =
        let date = "2022-04-01"
        let days = 20

        Storage.saveIndustrySMABreakdowns date ("airlines",days,10,50)
        |> ignore

        let updates = date |> Reports.getIndustrySMABreakdowns days

        let update = Assert.Single(updates)

        Assert.Equal("airlines", update.industry)
        Assert.Equal(10, update.breakdown.above)
        Assert.Equal(50, update.breakdown.below)

    
    [<Fact>]
    let ``latest industry sma breakdow works`` () =
        let update =
            StorageTests.testStockIndustry
            |> Reports.getMostRecentIndustrySMABreakdown 20

        match update with
            | Some update ->
                Assert.Equal(StorageTests.testStockIndustry, update.industry)
            | None ->
                Assert.True(false, "Expected industry sma breakdown to be found")

    [<Fact>]
    let ``latest sma breakdown date works`` () =
        let date = Reports.getIndustrySMABreakdownLatestDate()
        Assert.True(date > DateTime.MinValue)


    [<Fact>]
    let ``get industry trends for industry`` () =
        let trends = StorageTests.testStockIndustry |> Reports.getIndustrySMABreakdownsForIndustry 20
        Assert.NotEmpty(trends)

    [<Fact>]
    let ``get stock SMA breakdown works`` () =
        let (above20, below20) = Reports.getStockSMABreakdown 20
        let (above200, below200) = Reports.getStockSMABreakdown 200

        Assert.True(above20 > 0)
        Assert.True(below20 > 0)
        Assert.True(above200 > 0)
        Assert.True(below200 > 0)
        Assert.True(Math.Abs(above20 + below20 - above200 - below200) <= 1) // sometimes they are off by one, something finviz

    [<Fact>]
    let ``get daily SMA breakdowns works`` () =
        let results = Reports.getDailySMABreakdown 20 20
        Assert.NotEmpty(results)

    [<Fact>]
    let ``get industry trends works`` () =
        let results = Reports.getIndustryTrends()
        Assert.NotEmpty(results)

    [<Fact>]
    let ``calculate industry trends works`` () =
        let smaBreakdowns = Reports.getIndustrySMABreakdownsForIndustry 20 StorageTests.testStockIndustry
        let (streak,direction, change) = FinvizScraper.Core.IndustryTrendsCalculator.calculate smaBreakdowns

        Assert.True(streak > 0)
        Assert.True(direction = FinvizScraper.Core.TrendDirection.Up || direction = FinvizScraper.Core.TrendDirection.Down)
        Assert.True(change > -100m)
        Assert.True(change < 100m)

    [<Fact>]
    let ``get industry trend works`` () =
        let trend = Reports.getIndustryTrend 20 StorageTests.testStockIndustry
        Assert.True(trend.IsSome)
