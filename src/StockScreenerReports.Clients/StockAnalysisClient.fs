module StockScreenerReports.FinvizClient.StockAnalysisClient

open System
open System.Text.RegularExpressions
open StockScreenerReports.Core

let mutable outputFunc = (fun _ -> ())

let private fetchUrl (url:string) =
        // make ure that we sleep a bit before each request
        url |> outputFunc 
        let web = HtmlAgilityPack.HtmlWeb()
        web.Load(url)
        
let getCorporateActions() =
        
    let doc = fetchUrl "https://stockanalysis.com/actions/"
    
    let tableRows =
        doc.DocumentNode.Descendants("table")
        |> Seq.find (_.HasClass("svelte-1swpzu1"))
        |> _.Descendants("tbody")
        |> Seq.head
        |> _.Descendants("tr")

    tableRows
    |> Seq.map (fun row ->
        let cells = row.Descendants("td") |> Seq.toList
        let date = cells[0].InnerText.Trim()
        let symbol = cells[1].InnerText.Trim()
        let typeText = cells[2].InnerText.Trim()
        let action = cells[3].InnerText.Trim()

        let corporateActionType =
            match typeText with
            | "Delisted" -> Delisted
            | "Symbol Change" ->
                let regex = Regex(@"(.*) ticker symbol changed to (.*)")
                let matches = regex.Match(action)
                if matches.Success then
                    let oldSymbol = matches.Groups[1].Value
                    let newSymbol = matches.Groups[2].Value
                    SymbolChange(oldSymbol, newSymbol)
                else
                    failwith "Invalid Symbol Change format"
            | "Bankruptcy" -> Bankruptcy
            | "Acquisition" -> Acquisition
            | "Listed" -> Listed
            | "Stock Split" ->
                let regex = Regex(@"stock split: (\d+(?:\.\d+)?) for (\d+(?:\.\d+)?)")
                let matches = regex.Match(action)
                if matches.Success then
                    let oldShares = decimal matches.Groups[2].Value
                    let newShares = decimal matches.Groups[1].Value
                    StockSplit(oldShares, newShares)
                else
                    // we have seen this only once with a very specific instance, add that to exception
                    match date, symbol with
                    | "Feb 1, 2024", "NA" -> StockSplit(1m, 1m)
                    | _ ->
                        failwith $"Invalid Stock Split format: {date}:{symbol}:{typeText}:{action}"
            | "Spinoff" -> Spinoff
            | _ -> failwith "Unknown corporate action type"

        {
            Date = date |> DateTime.Parse
            Symbol = symbol
            Type = corporateActionType
            Action = action
        })
    |> Seq.toList