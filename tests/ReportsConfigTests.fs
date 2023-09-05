module ReportsConfigTests
open Xunit.Abstractions
open Xunit
open StockScreenerReports.Core
open FsUnit

type ReportsConfigTests(output:ITestOutputHelper) =

    [<Theory>]
    [<InlineData("2023-04-01")>] // weekend
    [<InlineData("2023-04-02")>] // weekend
    [<InlineData("2023-04-07")>] // holiday
    let ``Non trading days take into account holidays and weekends`` dateStr =
        dateStr |> System.DateTime.Parse |> ReportsConfig.isTradingDay |> should be False

    [<Theory>]
    [<InlineData("2023-04-03")>] // monday
    [<InlineData("2023-04-04")>] // tuesday
    [<InlineData("2023-04-05")>] // wednesday
    [<InlineData("2023-04-06")>] // thursday
    [<InlineData("2023-03-31")>] // Fruday (prev week)
    let ``Trading days are weekdays`` dateStr =
        dateStr |> System.DateTime.Parse |> ReportsConfig.isTradingDay |> should be True

    [<Fact>]
    let ``now() is the same as DateTime.Now by default``() =
        let now = System.DateTime.Now
        let configNow = ReportsConfig.now()
        
        now.Date |> should be (equal configNow.Date)

    [<Fact>]
    let ``now() can be overridden``() =
        let prevFunc = TimeFunctions.nowFunc
        TimeFunctions.nowFunc <- fun() -> System.DateTime.Now.AddDays(-1)
        let now = System.DateTime.Now
        let configNow = ReportsConfig.now()
        TimeFunctions.nowFunc <- prevFunc // restore func for other tests

        now.Date |> should not' (equal configNow.Date)