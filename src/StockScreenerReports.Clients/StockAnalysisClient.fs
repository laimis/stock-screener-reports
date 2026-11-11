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
        
// The site is built with Svelte, whose scoped css classes (e.g. "svelte-77tdkt") are hashes
// that change on every redeploy. Never rely on them, find the table by its header columns instead.
let private findActionsTable (doc: HtmlAgilityPack.HtmlDocument) =
    let tables = doc.DocumentNode.Descendants("table") |> Seq.toList

    let headersOf (table: HtmlAgilityPack.HtmlNode) =
        table.Descendants("th")
        |> Seq.map (fun th -> th.InnerText.Trim().ToLowerInvariant())
        |> Seq.toList

    let isActionsTable table =
        let headers = headersOf table
        ["date"; "symbol"; "type"; "action"] |> List.forall (fun h -> headers |> List.contains h)

    match tables |> List.tryFind isActionsTable with
    | Some table -> table
    | None ->
        // most likely cause is a changed page or a bot challenge (e.g. Cloudflare "Just a moment...")
        let title =
            doc.DocumentNode.SelectSingleNode("//title")
            |> Option.ofObj
            |> Option.map (fun n -> n.InnerText.Trim())
            |> Option.defaultValue "<no title>"
        failwith $"Corporate actions table not found (tables on page: {tables.Length}, page title: '{title}')"

let parseCorporateActions (doc: HtmlAgilityPack.HtmlDocument) =

    let tableRows =
        findActionsTable doc
        |> _.Descendants("tr")
        // header row has th cells only, skip anything that is not a full data row
        |> Seq.filter (fun row -> (row.Descendants("td") |> Seq.length) >= 4)

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

let getCorporateActions() =
    fetchUrl "https://stockanalysis.com/actions/" |> parseCorporateActions
