open FinvizScraper

let newHighs:ScreenerInput = {
    name = "New Highs, 1.5x volume, >$10";
    url = "https://finviz.com/screener.ashx?v=111&s=ta_newhigh&f=sh_avgvol_o100,sh_opt_optionshort,sh_price_o10,sh_relvol_o1.5,ta_perf_dup&ft=4&o=-volume"
}

let topGainers:ScreenerInput = {
    name = "Top Gainer (above $10, relvol 1+)";
    url = "https://finviz.com/screener.ashx?v=111&s=ta_topgainers&f=sh_price_o10,sh_relvol_o1&ft=4&o=-change"
}

let topLosers:ScreenerInput = {
    name = "Top Losers (above $10, relvol 1+)";
    url = "https://finviz.com/screener.ashx?v=111&s=ta_toplosers&f=sh_price_o10,sh_relvol_o1&ft=4&o=-change"
}

let screener = topGainers

let screenerResults = FinvizClient.getResults screener.url

let sectors = screenerResults |> Processing.resultBreakdown (fun a -> a.sector)
let industries = screenerResults |> Processing.resultBreakdown (fun a -> a.industry)
let countries = screenerResults |> Processing.resultBreakdown (fun a -> a.country) 

let html =
    screenerResults
    |> Rendering.renderScreenerResultsAsHtml 
        screener.name
        sectors
        industries
        countries

System.IO.File.WriteAllText("index.html", html)