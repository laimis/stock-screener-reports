module ReportsConfigTests
open Xunit.Abstractions
open Xunit
open StockScreenerReports.Core

type ReportsConfigTests(output:ITestOutputHelper) =

    [<Theory>]
    [<InlineData("2023-04-01")>] // weekend
    [<InlineData("2023-04-02")>] // weekend
    [<InlineData("2023-04-07")>] // holiday
    let ``Non trading days take into account holidays and weekends`` dateStr =
        let date = System.DateTime.Parse(dateStr)
        Assert.False(date |> ReportsConfig.isTradingDay)

    [<Theory>]
    [<InlineData("2023-04-03")>] // monday
    [<InlineData("2023-04-04")>] // tuesday
    [<InlineData("2023-04-05")>] // wednesday
    [<InlineData("2023-04-06")>] // thursday
    [<InlineData("2023-03-31")>] // Fruday (prev week)
    let ``Trading days are weekdays`` dateStr =
        let date = System.DateTime.Parse(dateStr)
        Assert.True(date |> ReportsConfig.isTradingDay)