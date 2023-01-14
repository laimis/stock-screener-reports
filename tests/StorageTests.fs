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
        output.WriteLine($"Connection string for tests: {cnnString}")
        Storage.configureConnectionString cnnString
        Storage.configureLogger (fun message -> output.WriteLine(message))

    let generateTicker() =
        "stock" + System.Guid.NewGuid().ToString() |> StockTicker.create 
    
    let generateScreener() =
        "screener" + System.Guid.NewGuid().ToString()
        
    let saveAndReturnTestStock ticker =
        Storage.saveStock ticker testStockName testStockSector testStockIndustry testStockCountry

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

    // write test for screener results
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

        let date = System.DateTime.UtcNow |> Utils.convertToDateString

        let updated = Storage.updateIndustryTrend "testindustry" date 7 Up 121m 20

        Assert.Equal(1, updated)
