open FinvizScraper

let screener:ScreenerInput = {
    name = "New Highs, sales growth, large volume up";
    url = "https://finviz.com/screener.ashx?v=111&s=ta_newhigh&f=fa_salesqoq_high,sh_avgvol_o200,sh_opt_optionshort,sh_price_o10,sh_relvol_o1.5,ta_perf_dup&ft=4&o=-volume"
}

let screenerResults = 
    screener.url
    |> FinvizClient.fetchScreenerHtml
    |> FinvizClient.parseScreenerHtml

let resultBreakdownHelper groupBy =
    Processing.resultBreakdown groupBy

let sectors = resultBreakdownHelper (fun a -> a.sector) screenerResults
let industries = resultBreakdownHelper (fun a -> a.industry) screenerResults
let countries = resultBreakdownHelper (fun a -> a.country) screenerResults

let html =
    screenerResults
    |> Rendering.renderScreenerResultsAsHtml 
        screener.name
        sectors
        industries
        countries

System.IO.File.WriteAllText("index.html", html)