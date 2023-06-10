module StorageTests

open Xunit
open Xunit.Abstractions
open StockScreenerReports.Core
open StockScreenerReports.Storage

let testScreenerName    = "New Highs, 1.5x volume, >$10"
let screenerUrl         = "https://finviz.com/screener.ashx?v=111&s=ta_newhigh&f=fa_salesqoq_high,sh_avgvol_o200,sh_opt_optionshort,sh_price_o10,sh_relvol_o1.5,ta_perf_dup&ft=4&o=-volume"
let testStockName       = "Apple Inc."
let testStockSector     = "Technology"
let testStockIndustry   = "Computer Hardware"
let testStockCountry    = "United States"
let testStockIndustryWithSpecialCharacters = "Furnishings, Fixtures & Appliances"

let dbEnvironmentVariableName = "SSR_CONNECTIONSTRING"

type StorageTests(output:ITestOutputHelper) =
    do
        let cnnString = System.Environment.GetEnvironmentVariable(dbEnvironmentVariableName)
        Storage.configureConnectionString cnnString
        Reports.configureConnectionString cnnString
        Storage.configureLogger (fun message -> output.WriteLine(message))

    let generateTicker() =
        "stock" + System.Guid.NewGuid().ToString() |> StockTicker.create 
    
    let generateScreener() =
        "screener" + System.Guid.NewGuid().ToString()

    let generateInt min max =
        let rnd = System.Random()
        rnd.Next(min, max)

    let generateNumber min max =
        generateInt min max |> decimal
        
    let saveAndReturnTestStock ticker =
        Storage.saveStock ticker testStockName testStockSector testStockIndustry testStockCountry 1_000_000m

    [<Fact>]
    let ``Getting stock works`` () =
        
        let ticker = generateTicker()

        let stockOrNone = Storage.getStockByTicker ticker
        Assert.True(stockOrNone.IsNone)

        let stock = saveAndReturnTestStock ticker

        Assert.True(stock.id > 0)
        Assert.Equal(ticker, stock.ticker)
        Assert.Equal(testStockName, stock.company)
        Assert.Equal(testStockSector, stock.sector)
        Assert.Equal(testStockIndustry, stock.industry)
        Assert.Equal(testStockCountry, stock.country)
        Assert.True(stock.marketCap > Some 0m)

        let deleted = Storage.deleteStock stock
        Assert.Equal(1, deleted)

        let subsequent = Storage.getStockByTicker ticker

        Assert.True(subsequent.IsNone)

    [<Fact>]
    let ``Storing and retrieving screener works`` () =
        
        let screenerName = generateScreener()

        let screener = Storage.getScreenerByName screenerName
        Assert.True(screener.IsNone)

        let screener = Storage.saveScreener screenerName screenerUrl

        Assert.True(screener.id > 0)
        Assert.Equal(screenerName, screener.name)

        let byNameLookup = Storage.getScreenerByName screenerName
        match byNameLookup with
            | Some screener ->
                Assert.Equal(screenerName, screener.name)
                Assert.Equal(screenerUrl, screener.url)
            | None ->
                Assert.True(false, "Expected screener to be found by name")

        let screeners = Storage.getScreeners()
        Assert.NotEmpty(screeners)

        let byIdLookup = Storage.getScreenerById screener.id
        match byIdLookup with
            | Some screener ->
                Assert.Equal(screenerName, screener.name)
                Assert.Equal(screenerUrl, screener.url)
            | None ->
                Assert.True(false, "Expected screener to be found by id")
        
        let deleted = Storage.deleteScreener screener
        Assert.Equal(0, deleted[0])
        Assert.Equal(1, deleted[1])

        let subsequent = Storage.getScreenerByName screenerName
        match subsequent with
            | Some _ ->
                Assert.True(false, "Expected screener to be deleted")
            | None ->
                Assert.True(true)


    [<Fact>]
    let ``Storing and retrieving screener results works`` () =
        
        let screenerName = generateScreener()
        let ticker = generateTicker()

        let screener = Storage.saveScreener screenerName screenerUrl

        let date = Utils.getRunDate()

        Storage.deleteScreenerResults screener date |> ignore

        let stock = saveAndReturnTestStock ticker
        
        let result = {
            ticker = stock.ticker;
            company = testStockName;
            sector = testStockSector;
            industry = testStockIndustry;
            country = testStockCountry;
            marketCap = 1_000_000_000m;
            price = 100m;
            change = 10m;
            volume = 1000;
        }

        let stored = Storage.saveScreenerResult screener date stock result

        Assert.Equal(1, stored)

        Storage.deleteScreenerResults screener date |> ignore
        Storage.deleteStock stock |> ignore
        Storage.deleteScreener screener |> ignore

    [<Fact>]
    let ``Storing results creates appropriate stock entries``() =
        let screenerName = generateScreener()
        let ticker = generateTicker()

        let screener = Storage.saveScreener screenerName screenerUrl

        let date = Utils.getRunDate()

        Storage.deleteScreenerResults screener date |> ignore

        let stock = Storage.getStockByTicker ticker
        Assert.True(stock.IsNone)

        let results = Reports.getScreenerResults screener.id date
        Assert.Empty(results)

        let result = {
            ticker = ticker;
            company = testStockName;
            sector = testStockSector;
            industry = testStockIndustry;
            country = testStockCountry;
            marketCap = generateNumber 1_000_000_000 2_000_000_000;
            price = generateNumber 1 100;
            change = generateNumber 1 10;
            volume = generateInt 1_000_000 10_000_000;
        }

        let results = (screener,[result])

        Storage.saveScreenerResults date results

        let stock = Storage.getStockByTicker ticker

        match stock with
            | Some stock ->
                Assert.Equal(ticker, stock.ticker)
                Assert.Equal(testStockName, stock.company)
                Assert.Equal(testStockSector, stock.sector)
                Assert.Equal(testStockIndustry, stock.industry)
                Assert.Equal(testStockCountry, stock.country)
                Assert.Equal(stock.marketCap, Some (result.marketCap))
            | None ->
                Assert.True(false, "Expected stock to be created")

        let screenerResults = Reports.getScreenerResults screener.id date
        Assert.NotEmpty(screenerResults)

        Storage.deleteScreenerResults screener date |> ignore
        Storage.deleteStock stock.Value |> ignore
        Storage.deleteScreener screener |> ignore

    [<Fact>]
    let ``get stocks by sector works`` () =
        let stocks = Storage.getStocksBySector testStockSector
        Assert.NotEmpty(stocks)

    [<Fact>]
    let ``get stocks by industry works`` () =
        let stocks = Storage.getStocksByIndustry testStockIndustry
        Assert.NotEmpty(stocks)

    [<Fact>]
    let ``get industries works`` () =
        let industries = Storage.getIndustries()
        Assert.NotEmpty(industries)

    [<Fact>]
    let ``save job works`` () =
        let message = "generated test results and it was great"
        let timestamp = System.DateTimeOffset.UtcNow

        let count = Storage.saveJobStatus TestJob timestamp Success message
        Assert.Equal(1, count)

        let (latestMessage,latestTimestamp) = 
            match (Storage.getLatestJobStatus TestJob) with
            | Some (message,timestamp) -> (message,timestamp)
            | None -> ("",System.DateTimeOffset.MinValue)

        Assert.Equal(message, latestMessage)
        Assert.Equal(timestamp.DateTime, latestTimestamp.DateTime, (System.TimeSpan.FromMilliseconds(1)))

    [<Fact>]
    let ``daily sma breakdown works`` () =

        let date = Reports.getIndustrySMABreakdownLatestDate()

        let updated = Storage.updateSMABreakdowns (date |> Utils.convertToDateString) 20

        Assert.Equal(1, updated)

    [<Fact>]
    let ``updating industry trend works`` () =

        // move it to the past so that it does not show up
        // in real app
        let date = System.DateTime.UtcNow.AddDays(-100)

        let trend = {streak=7;direction=Up;change=121m;value=100m;}

        let (industrySmaBreakdown:IndustrySMABreakdown) = {
            industry = "testindustry";
            breakdown = {
                date = date;
                above = 1;
                below = 1;
                days = 20;
            }
        }

        let updated = Storage.updateIndustryTrend industrySmaBreakdown trend

        Assert.Equal(1, updated)

    [<Fact>]
    let ``market cycle for industry works`` () =

        let range = ReportsConfig.dateRangeAsStrings()

        let trendAndCycle =
            testStockIndustry
            |> Reports.getIndustrySMABreakdownsForIndustry Constants.SMA20 range
            |> TrendsCalculator.calculateTrendAndCycleForIndustry
        
        let cycle = trendAndCycle.cycle

        Storage.saveIndustryCycle Constants.SMA20 cycle testStockIndustry |> ignore

        let (industry, saved) = Storage.getIndustryCycle Constants.SMA20 testStockIndustry

        Assert.Equal(cycle, saved)
        Assert.Equal(testStockIndustry, industry)

    [<Fact>]
    let ``get stocks for tickers works`` () =

        let tickers = ["AMD"; "NVDA"; "AMAT"]

        let stocks = tickers |> Storage.getStockByTickers

        Assert.Equal(3, stocks.Length)

    [<Fact>]
    let ``find stocks with no match query works`` () =

        let stocks = Storage.findStocksByTicker "nomatches"

        Assert.Empty(stocks)

    [<Fact>]
    let ``find stocks with match query works`` () =

        let stocks = Storage.findStocksByTicker "AM"

        Assert.NotEmpty(stocks)

        let indexOfAMD = List.findIndex (fun x -> x.ticker |> StockTicker.value = "AMD") stocks

        Assert.True(indexOfAMD >= 0)