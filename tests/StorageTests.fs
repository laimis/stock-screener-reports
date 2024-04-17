module StorageTests

open System
open System.IO
open Xunit
open Xunit.Abstractions
open StockScreenerReports.Core
open StockScreenerReports.Storage
open FsUnit

[<Literal>]
let testScreenerName    = "New Highs, 1.5x volume, >$10"
[<Literal>]
let screenerUrl         = "https://finviz.com/screener.ashx?v=111&s=ta_newhigh&f=fa_salesqoq_high,sh_avgvol_o200,sh_opt_optionshort,sh_price_o10,sh_relvol_o1.5,ta_perf_dup&ft=4&o=-volume"
[<Literal>]
let testStockName       = "Apple Inc."
[<Literal>]
let testStockSector     = "Technology"
[<Literal>]
let testStockIndustry   = "Computer Hardware"
[<Literal>]
let testStockCountry    = "USA"
[<Literal>]
let testStockIndustryWithSpecialCharacters = "Furnishings, Fixtures & Appliances"
[<Literal>]
let dbEnvironmentVariableName = "SSR_CONNECTIONSTRING"

module SecretsHelper =
    let getSecret key =
        let variable = Environment.GetEnvironmentVariable(key)
        match variable with
        | null ->
            // try to read it from file system
            let path = Path.Combine("..\\..\\..\\..\\", $"{key}_secret.txt")
            match File.Exists(path) with
            | true -> File.ReadAllText(path)
            | false -> variable
        | _ ->
            variable
            
type StorageTests(output:ITestOutputHelper) =
    do
        let cnnString = SecretsHelper.getSecret(dbEnvironmentVariableName)
        Storage.configureConnectionString cnnString
        Reports.configureConnectionString cnnString
        Storage.configureLogger output.WriteLine

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
        stockOrNone |> should equal None

        let stock = saveAndReturnTestStock ticker

        stock.id |> should be (greaterThan 0)
        stock.ticker |> should equal ticker
        stock.company |> should equal testStockName
        stock.sector |> should equal testStockSector
        stock.industry |> should equal testStockIndustry
        stock.country |> should equal testStockCountry
        stock.marketCap |> should equal (Some 1_000_000m)

        let deleted = Storage.deleteStock stock
        deleted |> should equal 1
        
        let subsequent = Storage.getStockByTicker ticker
        subsequent |> should equal None

    [<Fact>]
    let ``Storing and retrieving screener works`` () =
        
        let screenerName = generateScreener()

        screenerName |> Storage.getScreenerByName |> should equal None

        let screener = Storage.createScreener screenerName screenerUrl

        screener.id |> should be (greaterThan 0)
        screener.name |> should equal screenerName

        let byNameLookup = Storage.getScreenerByName screenerName
        match byNameLookup with
            | Some screener ->
                screener.name |> should equal screenerName
                screener.url |> should equal screenerUrl
            | None ->
                byNameLookup |> should not' (equal None)

        let screeners = Storage.getScreeners()
        screeners |> should not' (be Empty)

        let byIdLookup = Storage.getScreenerById screener.id
        match byIdLookup with
            | Some screener ->
                screener.name |> should equal screenerName
                screener.url |> should equal screenerUrl
            | None ->
                byIdLookup |> should not' (equal None)
        
        let deleted = Storage.deleteScreener screener
        deleted[0] |> should equal 0
        deleted[1] |> should equal 1

        let subsequent = Storage.getScreenerByName screenerName
        subsequent |> should equal None

    [<Fact>]
    let ``Storing and retrieving screener results works`` () =
        
        let screenerName = generateScreener()
        let ticker = generateTicker()

        let screener = Storage.createScreener screenerName screenerUrl

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

        Storage.saveScreenerResult screener date stock result |> should equal 1

        Storage.deleteScreenerResults screener date |> ignore
        Storage.deleteStock stock |> ignore
        Storage.deleteScreener screener |> ignore

    [<Fact>]
    let ``Storing results creates appropriate stock entries``() =
        let screenerName = generateScreener()
        let ticker = generateTicker()

        let screener = Storage.createScreener screenerName screenerUrl

        let date = Utils.getRunDate()

        Storage.deleteScreenerResults screener date |> ignore

        let stock = Storage.getStockByTicker ticker
        stock |> should equal None

        let results = Reports.getScreenerResults screener.id date
        results |> should be Empty

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

        let stockOption = Storage.getStockByTicker ticker
        stockOption |> should not' (equal None)

        let stock = stockOption.Value
        stock.ticker |> should equal ticker
        stock.company |> should equal testStockName
        stock.sector |> should equal testStockSector
        stock.industry |> should equal testStockIndustry
        stock.country |> should equal testStockCountry
        stock.marketCap |> should equal (Some result.marketCap)

        let screenerResults = Reports.getScreenerResults screener.id date
        screenerResults |> should not' (be Empty)

        Storage.deleteScreenerResults screener date |> ignore
        Storage.deleteStock stockOption.Value |> ignore
        Storage.deleteScreener screener |> ignore

    [<Fact>]
    let ``get stocks by sector works`` () =
        let stocks = Storage.getStocksBySector testStockSector
        stocks |> should not' (be Empty)

    [<Fact>]
    let ``get stocks by industry works`` () =
        let stocks = Storage.getStocksByIndustry testStockIndustry
        stocks |> should not' (be Empty)
        
    [<Fact>]
    let ``get stocks by country works`` () =
        let stocks = Storage.getStocksByCountry testStockCountry
        stocks |> should not' (be Empty)

    [<Fact>]
    let ``get industries works`` () =
        let industries = Storage.getIndustries()
        industries |> should not' (be Empty)
        
    [<Fact>]
    let ``get countries works`` () =
        let countries = Storage.getCountries()
        countries |> should not' (be Empty)

    [<Fact>]
    let ``save job works`` () =
        let message = "generated test results and it was great"
        let timestamp = ReportsConfig.now()

        let count = Storage.saveJobStatus TestJob timestamp Success message
        count |> should equal 1

        let testJob = Storage.getJobs() |> List.filter (fun j -> j.name = TestJob) |> List.head

        testJob.name |> should equal TestJob
        testJob.status |> should equal Success
        testJob.message |> should equal message

        let timestampDiff = timestamp - testJob.timestamp
        timestampDiff.TotalSeconds |> should (equalWithin 1) 0

    [<Fact>]
    let ``daily sma breakdown works`` () =

        let date = Reports.getIndustrySMABreakdownLatestDate()

        SMA20 |> Storage.updateIndustrySMABreakdowns (date |> Utils.convertToDateString) |> should equal 1

    [<Fact>]
    let ``updating industry trend works`` () =

        // move it to the past so that it does not show up
        // in real app
        let date = ReportsConfig.now().AddDays(-100)

        let trend = {streak=7;direction=Up;change=121m;value=100m;}

        let (industrySmaBreakdown:IndustrySMABreakdown) = {
            industry = "testindustry";
            breakdown = {
                date = date;
                above = 1;
                below = 1;
                days = SMA20;
            }
        }

        Storage.updateIndustryTrend industrySmaBreakdown trend |> should equal 1

    [<Fact>]
    let ``market cycle for industry works`` () =

        let range = ReportsConfig.dateRangeAsStrings()

        let trendAndCycle =
            testStockIndustry
            |> Reports.getIndustrySMABreakdownsForIndustry SMA20 range
            |> TrendsCalculator.calculateForIndustry
        
        let cycle = trendAndCycle.cycle

        Storage.saveIndustryCycle SMA20 cycle testStockIndustry |> ignore

        let (industry, saved) = Storage.getIndustryCycle SMA20 testStockIndustry

        saved |> should equal cycle
        industry |> should equal testStockIndustry

    [<Fact>]
    let ``get stocks for tickers works`` () =

        ["AMD"; "NVDA"; "AMAT"]
        |> Storage.getStockByTickers
        |> List.length
        |> should equal 3

    [<Fact>]
    let ``find stocks with no match query works`` () =

        "nomatches" |> Storage.findStocksByTicker |> should be Empty

    [<Fact>]
    let ``find stocks with match query works`` () =

        let stocks = Storage.findStocksByTicker "AM"

        stocks |> List.map (fun x -> x.ticker |> StockTicker.value) |> should contain "AMD"

    [<Fact>]
    let ``find stocks with name matches query works`` () =
        let stocks = Storage.findStocksByTickerOrName "Advanced"

        stocks |> List.map (fun x -> x.ticker |> StockTicker.value) |> should contain "AMD"
        
    
    [<Fact>]
    let ``storing industry sequences works``() =
        
        let industry = "Technology"
        let sequences = [
            {
                industry = industry
                values = [
                    { date = DateTime(2023, 1, 10); value = 92m }
                    { date = DateTime(2023, 1, 5); value = 98m }
                    { date = DateTime(2023, 1, 1); value = 95m }
                ]
                open' = false
                type' = High 
            }
            {
                industry = industry
                open' = true
                values = [
                    { date = DateTime(2023, 2, 10); value = 96m }
                    { date = DateTime(2023, 2, 5); value = 93m }
                    { date = DateTime(2023, 2, 1); value = 90m }
                ]
                type' = High
            }
            {
                industry = industry
                open' = true
                values = [
                    { date = DateTime(2023, 3, 10); value = 0m }
                    { date = DateTime(2023, 3, 5); value = 7m }
                    { date = DateTime(2023, 3, 1); value = 8m }
                ]
                type' = Low
            }
        ]

        // Save the sequences
        sequences |> List.iter Storage.saveIndustrySequenceWithPoints
        
        // Get the sequences
        let savedSequences = Storage.getIndustrySequencesForIndustry industry
        
        // Check the sequences
        savedSequences |> List.length |> should equal 3
        savedSequences |> List.map (_.industry) |> should contain industry
        savedSequences |> List.map (_.type') |> should contain High
        savedSequences |> List.map (_.type') |> should contain Low
        
    [<Fact>]
    let ``saving and getting alerts works``() =
        
        let alert = {
            date = DateTime.UtcNow
            sentiment = Positive
            description = "this alert is great, get going on it!"
            strength = 5m
            acknowledged = false
            alertType = IndustryAlert("Aluminum") 
        }
        
        alert |> Storage.saveAlert |> should equal 1
        
        let alerts = Storage.getAlerts()
        
        alerts |> List.length |> should equal 1
        
        let savedAlert = alerts |> List.head
        
        savedAlert.date.Date |> should equal alert.date.Date
        savedAlert.sentiment |> should equal alert.sentiment
        savedAlert.description |> should equal alert.description
        savedAlert.strength |> should equal alert.strength
        savedAlert.acknowledged |> should equal alert.acknowledged
        savedAlert.alertType |> should equal alert.alertType
        
        // saving it second time should work
        alert |> Storage.saveAlert |> should equal 1
        // now acknowledge it
        let updatedAlert = { savedAlert with acknowledged = true }
        
        updatedAlert |> Storage.saveAlert |> should equal 1
        
        let updatedAlerts = Storage.getAlerts()
        
        updatedAlerts |> List.length |> should equal 0 // because it is acknowledged
        
        Storage.deleteAlert alert |> should equal 1