module ReportTests

open StorageTests
open Xunit
open System
open StockScreenerReports.Storage
open StockScreenerReports.Core
open FsUnit

type ReportTests() =
    // output:ITestOutputHelper - add this above if you need output
    do
        Reports.configureConnectionString (SecretsHelper.getSecret(dbEnvironmentVariableName))
        Storage.configureConnectionString (SecretsHelper.getSecret(dbEnvironmentVariableName))

    let getTestScreener = 
        Storage.getScreenerByName StorageTests.testScreenerName

    let getTestSector = "Energy"
    let getTestIndustries = ["Agricultural Inputs";"Biotechnology";"Semiconductors"]
    let getTestCountry = "USA"

    let getTestStartDate() = ReportsConfig.now().AddDays(-7)
    let getTestEndDate() = ReportsConfig.now()

    let topGroupingTest resultGenerator containsMember =

        let screener = getTestScreener
        
        match screener with
            | Some screener ->
                let grouping = screener |> resultGenerator |> Seq.map (fun (name, _) -> name)
                grouping |> should not' (be Empty)
                grouping |> should contain containsMember

            | None ->
                screener |> should not' (equal None)

    let parseDate dateString =
        DateTime.ParseExact(dateString, "yyyy-MM-dd", Globalization.CultureInfo.InvariantCulture)
        
    let getTrend trendWithCycle = trendWithCycle.trend

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
    let ``Getting top countries by date works`` dateStr =
        let date = parseDate dateStr
        "USA" |> topGroupingTest (fun x-> Reports.topCountries x.id date)

    [<Fact>]
    let ``Screener result list works``() =
        
        let screenerSet = 
            Storage.getScreeners()
            |> List.map (fun x -> x.id)
            |> Set.ofList

        let screenerResults = Reports.getLatestScreenerResults()

        let filteredList =
            screenerResults
            |> List.filter (fun x -> screenerSet.Contains(x.screenerid) |> not)

        filteredList |> should be Empty

    [<Fact>]
    let ``Particular screener results list works``() =

        let screener = Reports.getLatestScreenerResults().Head

        let results = screener.date |> Utils.convertToDateString |> Reports.getScreenerResults screener.screenerid

        results |> should not' (be Empty)

    [<Fact>]
    let ``Particular screener result for multiple days works``() =

        let screener = Reports.getLatestScreenerResults().Head

        let dayRange = ReportsConfig.dateRangeAsStrings()
        
        let results = screener.screenerid |> Reports.getScreenerResultsForDays dayRange

        results |> should not' (be Empty)

    [<Fact>]
    let ``Particular screener daily counts work``() =

        let screener = getTestScreener
        let dateRange = ReportsConfig.dateRangeAsStrings()

        let results = screener.Value.id |> Reports.getDailyCountsForScreener dateRange

        results |> should not' (be Empty)

        let _,firstCount = results.Item(0)
        firstCount |> should be (greaterThan 0)

    [<Fact>]
    let ``Particular screener daily volume works``() =

        let screener = getTestScreener
        let dateRange = ReportsConfig.dateRangeAsStrings()

        let results = screener.Value.id |> Reports.getDailyAverageVolumeForScreener dateRange   

        results |> should not' (be Empty)

        let _,firstCount = results.Item(0)
        
        firstCount |> should be (greaterThan 0)

    [<Fact>]
    let ``Screener results for stock and date range works`` () =

        let start = DateTime(2023, 1, 1)
        let ending = DateTime(2023, 2, 26)

        let range = (start, ending) |> ReportsConfig.formatDateRangeToStrings 
        let ticker = "ACLS" |> StockTicker.create
        let results = range |> Reports.getScreenerResultsForTickerDayRange ticker

        results |> should not' (be Empty)

    [<Fact>]
    let ``Date range sector grouping works``() =
        let start = DateTime(2022, 6, 1)
        let ending = DateTime(2022, 6, 30)
        let range = ReportsConfig.formatDateRangeToStrings (start, ending)

        "Energy" |> topGroupingTest (
            fun x -> 
                Reports.topSectorsOverDays x.id range
            )

    [<Fact>]
    let ``Date range industry grouping works``() =
        let start = DateTime(2022, 6, 1)
        let ending = DateTime(2022, 6, 30)
        let range = ReportsConfig.formatDateRangeToStrings (start, ending)

        "Biotechnology" |> topGroupingTest (
            fun x -> 
                Reports.topIndustriesOverDays x.id range
            )

    [<Fact>]
    let ``Date range country grouping works``() =
        let range = ReportsConfig.dateRangeWithDays(7) |> ReportsConfig.formatDateRangeToStrings
        "USA" |> topGroupingTest (fun x -> Reports.topCountriesOverDays x.id range)

    [<Fact>]
    let ``getting screener results for ticker works``() =
        let results = "cutr" |> StockTicker.create |> Reports.getScreenerResultsForTicker 100 
        results |> should not' (be Empty)

    [<Fact>]
    let ``getting daily counts for screeners filtered by sector works``() =
        let screener = Constants.TopGainerScreenerId

        let dayRange = ReportsConfig.dateRangeAsStrings()

        let screenerResult = screener |> Reports.getScreenerResultsForDays dayRange

        let sector = screenerResult.Item(0).sector 

        let results = Reports.getDailyCountsForScreenerAndSector screener sector dayRange
        
        results |> should not' (be Empty)

    [<Fact>]
    let ``getting daily counts for screeners filtered by industry works``() =
        let screener = getTestScreener

        let dateRange = ReportsConfig.dateRangeAsStrings()

        // try several industries, we should fine at least one that has results
        let exists =
            getTestIndustries
            |> List.map (fun industry -> Reports.getDailyCountsForScreenerAndIndustry screener.Value.id industry dateRange)
            |> List.exists (fun results -> results |> Seq.length > 0)
        
        exists |> should be True

    [<Fact>]
    let ``getting daily counts for screeners filtered by country works``() =
        let screener = getTestScreener
        let industry = getTestCountry
        let dateRange = ReportsConfig.dateRangeAsStrings()

        let results = Reports.getDailyCountsForScreenerAndCountry screener.Value.id industry dateRange
        
        results |> should not' (be Empty)

    [<Fact>]
    let ``getting trending industries works``() =
        let results = 
            [Constants.NewHighsScreenerId]
            |> Reports.getTopIndustriesForScreeners 14

        results |> should not' (be Empty)

    [<Fact>]
    let ``getting trending sectors works``() =
        let results = 
            Constants.NewHighsScreenerId
            |> Reports.getTopSectorsForScreener 14

        results |> should not' (be Empty)

    [<Fact>]
    let ``getting country breakdowns works``() =
        let results = Reports.getStockByCountryBreakdown()

        results |> should not' (be Empty)

        let country,count = results.Item(0)

        country |> should equal "USA"
        count |> should be (greaterThan 1000)
        count |> should be (lessThan 8000)

    [<Fact>]
    let ``industry sma breakdowns end to end works`` () =
        let date = "2022-04-01"
        let sma = SMA20

        Storage.saveIndustrySMABreakdowns date ("airlines",sma,10,50)
        |> ignore

        let updates = date |> Reports.getIndustrySMABreakdownsForDate sma

        updates.Length |> should equal 1

        let update = updates |> List.head

        update.industry |> should equal "airlines"
        update.breakdown.above |> should equal 10
        update.breakdown.below |> should equal 50
    
    [<Fact>]
    let ``latest industry sma breakdow works`` () =
        let update =
            StorageTests.testStockIndustry
            |> Reports.getMostRecentIndustrySMABreakdown SMA20

        match update with
            | Some update ->
                update.industry |> should equal StorageTests.testStockIndustry
            | None ->
                update |> should not' (equal None)

    [<Fact>]
    let ``latest sma breakdown date works`` () =
        let date = Reports.getIndustrySMABreakdownLatestDate()
        date |> should be (greaterThan DateTime.MinValue)

    [<Fact>]
    let ``get industry trends for industry`` () =
        let dateRange = ReportsConfig.dateRangeAsStrings()
        let trends =
            StorageTests.testStockIndustry
            |> Reports.getIndustrySMABreakdownsForDateRange SMA20 dateRange
        
        trends |> should not' (be Empty)

    [<Fact>]
    let ``get stock SMA breakdown works`` () =
        let breakdowns =
            SMA.All |> List.map (fun sma -> Reports.getStockSMABreakdown sma)

        // we collapse tuples into array and check each member to be above 0
        breakdowns
            |> List.collect (fun pair -> [pair |> fst; pair |> snd])
            |> List.iter (fun breakdown ->
                breakdown |> should be (greaterThan 0)
            )

        // we also add above and belows for both smas and make sure they are close
        let sums = breakdowns |> List.map (fun pair -> (pair |> fst) + (pair |> snd))

        let pairwise = 
            sums
            |> List.pairwise
            |> List.map (fun (a,b) -> 
                Math.Abs(a - b)
            )
            |> List.head

        pairwise |> should be (lessThanOrEqualTo 1) // sometimes they are off by one, something finviz

    [<Fact>]
    let ``get daily SMA breakdowns works`` () =
        let results = Reports.getDailySMABreakdown (ReportsConfig.dateRangeAsStrings()) SMA20
        results |> should not' (be Empty)

        // check order
        let first = results.Item(0)
        let last = results.Item(results.Length - 1)

        first.date |> should be (lessThan last.date)

    [<Fact>]
    let ``get industry trends works`` () =
        let latestDate = Reports.getIndustrySMABreakdownLatestDate()
        let formattedDate = latestDate |> Utils.convertToDateString

        let results = SMA200 |> Reports.getIndustryTrends formattedDate
        
        results |> should not' (be Empty)

    [<Fact>]
    let ``industry trends breakdown contain both up and down`` () =

        let date = "2023-03-11"

        let dateToUse = 
            date
            |> Reports.getIndustryTrendsLastKnownDateAsOf 
            |> Option.get |> Utils.convertToDateString

        SMA.All
        |> List.iter (fun days ->
            let up, down = Reports.getIndustryTrendBreakdown dateToUse days
            up |> should be (greaterThan 0)
            down |> should be (greaterThan 0)
        )

    [<Fact>]
    let ``calculate industry trends works`` () =
        let smaBreakdowns =
            StorageTests.testStockIndustry
            |> Reports.getIndustrySMABreakdownsForDateRange SMA20 (ReportsConfig.dateRangeAsStrings())
        let trend = smaBreakdowns |> TrendsCalculator.calculateForIndustry |> getTrend

        trend.streak |> should be (greaterThan 0)
        let directionIsEitherUpOrDown = trend.direction = Up || trend.direction = Down
        directionIsEitherUpOrDown |> should be True
        trend.change |> should be (greaterThan -100m)
        trend.change |> should be (lessThan 100m)

    [<Fact>]
    let ``get industry trend works`` () =
        let dateToUseOpt = ReportsConfig.dateRangeAsStrings() |> snd |> Reports.getIndustryTrendsLastKnownDateAsOf
        let dateToUse = dateToUseOpt |> Option.get |> Utils.convertToDateString
        let trend = Reports.getIndustryTrend SMA20 dateToUse testStockIndustry
        trend.IsSome |> should be True

    [<Fact>]
    let ``get tickers for screener and date range works`` () =

        let dateRange = (
            ReportsConfig.now().AddDays(-30) |> Utils.convertToDateString,
            ReportsConfig.now() |> Utils.convertToDateString
        )

        let results = Constants.NewHighsScreenerId |> Reports.getTickersWithScreenerResultsForDateRange dateRange

        results |> should not' (be Empty)
        
        
    let detectTrend shortPeriod longPeriod (industryBreakdowns:SMABreakdown seq) =
        let toSMA (prices:decimal array) interval =
        
            let sma = Array.create prices.Length None
            
            for i in 0..prices.Length-1 do
                if i < interval then
                    sma[i] <- None
                else
                    let sum = 
                        [i-interval..i-1]
                        |> Seq.map (fun j -> prices[j])
                        |> Seq.sum
                        
                    sma[i] <- Some (Math.Round(sum / decimal interval, 2))
                    
            sma
            
        let percentagesAboveSMA =
            industryBreakdowns
            |> Seq.map (fun breakdown -> breakdown.date, breakdown.percentAbove)
            |> Seq.sortBy fst
            |> Seq.map snd
            |> Seq.toArray

        let shortSMA = toSMA percentagesAboveSMA shortPeriod |> Array.last |> Option.get
        let longSMA = toSMA percentagesAboveSMA longPeriod |> Array.last |> Option.get

        let trend = if shortSMA > longSMA then 1m else -1m
        let strength = abs (shortSMA - longSMA)

        trend * strength

    [<Fact>]
    let ``trend calculations work``() =
        
        // get industry breakdowns for the reference date
        let referenceDate = "2024-03-22"
        let breakdowns = Reports.getIndustrySMABreakdownsForDate SMA20 referenceDate
        
        // from there we will get all the industries that have data
        let industries = breakdowns |> List.map _.industry
        
        let industriesWithTrend =
            industries
            |> List.map (fun industry ->
                let trend =
                    industry
                    |> Reports.getIndustrySMABreakdownsForDateRange SMA20 ("2024-01-01", referenceDate)
                    |> TrendsCalculator.calculateSMACrossOverStrength
                    
                industry, trend
            )
            |> List.sortByDescending snd
            
        industriesWithTrend |> should not' (be Empty)
        
        // first should be the most positive
        let first = industriesWithTrend |> List.head
        let last = industriesWithTrend |> List.last
        
        first |> (fun (_,trend) -> trend > 0m) |> should be True
        last |> (fun (_,trend) -> trend < 0m) |> should be True
        
        // we should have at least 10 industries
        industriesWithTrend.Length |> should be (greaterThanOrEqualTo 10)
        first |> fst |> should be (equal "Uranium")
        last |> fst |> should be (equal "Paper & Paper Products")
        