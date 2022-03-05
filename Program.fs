open HtmlAgilityPack
open System
// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"


let url = "https://finviz.com/screener.ashx?v=111&s=ta_newhigh&f=fa_salesqoq_high,sh_avgvol_o200,sh_opt_optionshort,sh_price_o10,sh_relvol_o1.5,ta_perf_dup&ft=4&o=-volume"

let web = HtmlWeb()
web.CachePath <- "cache"
web.UsingCache <- true

let doc = web.Load(url)

// System.IO.File.WriteAllText("index.html", doc.ParsedText)

let nodes = doc.DocumentNode.SelectNodes("//table[@id='screener-views-table']/tr")

// 3rd node has the tickers

nodes 
    |> Seq.skip 3 // skip three tr nodes
    |> Seq.take 1 // take one that has the tickers table
    |> Seq.collect (fun n -> n.ChildNodes) // this should contain #text, <td>, #text
    |> Seq.skip 1 // skip one #text
    |> Seq.take 1 // take one that has td
    |> Seq.collect (fun n -> n.ChildNodes) // this should contain #text <table> #text
    |> Seq.skip 1 // skip one #text
    |> Seq.take 1 // take table
    |> Seq.collect (fun n -> n.ChildNodes)
    |> Seq.skip 1 // skip header row
    |> Seq.iter (fun n -> Console.WriteLine(n.Name))
