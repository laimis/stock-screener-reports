open FinvizScraper

let url = "https://finviz.com/screener.ashx?v=111&s=ta_newhigh&f=fa_salesqoq_high,sh_avgvol_o200,sh_opt_optionshort,sh_price_o10,sh_relvol_o1.5,ta_perf_dup&ft=4&o=-volume"

let html = 
    url
    |> FinvizClient.fetchScreenerHtml
    |> FinvizClient.parseScreenerHtml
    |> Rendering.generateHtml

System.IO.File.WriteAllText("index.html", html)