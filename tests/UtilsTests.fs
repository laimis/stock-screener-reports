module UtilsTests

open Xunit
open Xunit.Abstractions
open System
open StockScreenerReports.Core
open FsUnit

type UtilsTests(output:ITestOutputHelper) =

    [<Fact>]
    let ``runDate tests`` () =
        let todayExplicitly = ReportsConfig.now() |> Utils.convertToDateString
        let todayImplicitly = Utils.getRunDate()

        todayExplicitly |> should equal todayImplicitly

    [<Theory>]
    [<InlineData("A & E", "ae")>]
    [<InlineData("Technology", "technology")>]
    let ``cleanIndustry tests`` (industry:string) (expected:string) =
        let cleaned = Utils.cleanIndustry industry

        cleaned |> should equal expected

    [<Fact>]
    let ``add business days to Friday should return monday`` () =

        let friday = DateTime.ParseExact("2022-04-01", "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture)
        let nextBusinessDay = Utils.addDaysToClosestBusinessDay friday 1

        nextBusinessDay |> Utils.convertToDateString |> should equal "2022-04-04"

    [<Fact>]
    let ``subtract business days to Monday should return Friday`` () =

        let monday = DateTime.ParseExact("2022-04-04", "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture)
        let previousBusinessDay = Utils.subtractDaysToClosestBusinessDay monday 1

        previousBusinessDay |> Utils.convertToDateString |> should equal "2022-04-01"