module FinvizClientTests

open Xunit
open FinvizScraper
open Xunit.Abstractions

type ParsingTests(output:ITestOutputHelper) =

    // all time high screener
    let url = "https://finviz.com/screener.ashx?v=111&s=ta_newhigh&f=fa_salesqoq_high,sh_avgvol_o200,sh_opt_optionshort,sh_price_o10,sh_relvol_o1.5,ta_perf_dup&ft=4&o=-volume"

    [<Fact>]
    let ``End to end fetch works`` () =
        let results = FinvizClient.getResults url
        Assert.NotEmpty(results)