namespace StockScreenerReports.FinvizClient

module FinvizParsing =
    open StockScreenerReports.Core

    let mutable outputFunc = (fun _ -> ())

    let setOutputFunc (f:string -> unit) =
        outputFunc <- f

    let parseResultCount (doc:HtmlAgilityPack.HtmlDocument) =
        // SelectNodes/SelectSingleNode return null when nothing matches
        let legacyTotalText () =
            match doc.DocumentNode.SelectNodes("//table[@id='screener-views-table']/tr") with
            | null -> None
            | rows when rows.Count < 3 -> None
            | rows ->
                let row = rows.Item(2)
                match row.SelectNodes("//div[@id='screener-total']") with  // not always present, sometimes it has a table with rows indicating total
                | null ->
                    match row.SelectNodes("//td[@class='count-text']") with
                    | null -> None
                    | cells -> Some (cells.Item(0).InnerText)
                | totals -> Some (totals.Item(0).InnerText)

        // newer finviz layout (/screener/stocks): <p class="fv-table_toolbar-count">#1 / 21 Total</p>
        let newTotalText () =
            match doc.DocumentNode.SelectSingleNode("//p[contains(@class,'fv-table_toolbar-count')]") with
            | null -> None
            | node -> Some node.InnerText

        let totalText =
            match legacyTotalText () |> Option.orElseWith newTotalText with
            | Some text -> text
            | None ->
                let title =
                    match doc.DocumentNode.SelectSingleNode("//title") with
                    | null -> "<no title>"
                    | t -> t.InnerText.Trim()
                failwith $"Could not find result count in response (page title: '{title}', length: {doc.DocumentNode.OuterLength})"

        outputFunc totalText

        let removeTotalMarker (input:string) =
            input.Replace("Total","")

        match totalText with
            | x when x.Contains("#") ->  // the response could be Total: 4 #1
                let total = x.Substring(x.IndexOf("/") + 1)
                System.Int32.Parse(total |> removeTotalMarker)
            | _ -> System.Int32.Parse(totalText |> removeTotalMarker)

    let parseScreenerHtml (doc:HtmlAgilityPack.HtmlDocument) =

        let skipAndTake skip take seq =
            seq 
            |> Seq.skip skip
            |> Seq.take take

        let extractValueFromScreenerCell (node:HtmlAgilityPack.HtmlNode) =
            let value = node.ChildNodes[0].InnerText
            value

        let (|Decimal|_|) str =
            match System.Decimal.TryParse(str:string) with
            | (true,dec) -> Some(dec)
            | _ -> None


        let processScreenerRow (node:HtmlAgilityPack.HtmlNode) : Option<ScreenerResult> =
            let toDecimal str =
                match str with
                | Decimal dec -> dec
                | _ -> raise (new System.Exception("toDecimal conversion failed for " + str))

            let fromCapToDecimal (value:string) =
                match value with
                | "-" -> 0m // sometimes when it does know know cap, it returns -
                | _ ->
                    let lastChar = value[value.Length - 1]
                    let numericPortion = 
                        match value.Substring(0, value.Length - 1) with
                        | Decimal dec -> dec
                        | _ -> raise (new System.Exception("fromCap numeric conversion failed for " + value))

                    match lastChar with
                    | 'M' -> numericPortion * 1000000m
                    | 'B' -> numericPortion * 1000000000m
                    | _   -> raise (new System.Exception("Cap to decimal conversion failed for " + value))

            let toInt str =
                try
                    System.Int64.Parse(str)
                with
                | _ -> raise (new System.Exception("toInt conversion failed for " + str))

            let remove characterToRemove str =
                String.filter (fun c -> c.Equals(characterToRemove) |> not) str

            match node.ChildNodes.Count with
            | 0 -> None
            | _ ->
                let tickerNode = node.ChildNodes[2] |> extractValueFromScreenerCell |> StockTicker.create
                let companyNode = extractValueFromScreenerCell node.ChildNodes[3]
                let sectorNode = extractValueFromScreenerCell node.ChildNodes[4]
                let industryNode = extractValueFromScreenerCell node.ChildNodes[5]
                let countryNode = extractValueFromScreenerCell node.ChildNodes[6]
                let marketCapNode = extractValueFromScreenerCell node.ChildNodes[7] |> fromCapToDecimal
                let priceNode = extractValueFromScreenerCell node.ChildNodes[9] |> toDecimal
                let changeNode = extractValueFromScreenerCell node.ChildNodes[10] |> remove '%' |> toDecimal
                let volumeNode = extractValueFromScreenerCell node.ChildNodes[11] |> remove ',' |> toInt
                Some {
                    ticker=tickerNode;
                    company=companyNode;
                    sector=sectorNode;
                    industry=industryNode;
                    country=countryNode;
                    marketCap=marketCapNode;
                    price=priceNode;
                    change=changeNode;
                    volume=volumeNode
                }

        // newer finviz layout (/screener/stocks): rows are <div class="fv-table_row" data-row-key="TICKER">
        // with cells identified by data-column-id, which is much less brittle than column positions
        let processNewScreenerRow (row:HtmlAgilityPack.HtmlNode) : Option<ScreenerResult> =
            let cellText columnId =
                match row.SelectSingleNode($".//div[@data-column-id='{columnId}']") with
                | null -> raise (new System.Exception("Could not find column " + columnId + " in screener row"))
                | cell -> HtmlAgilityPack.HtmlEntity.DeEntitize(cell.InnerText).Trim()

            let toDecimal (str:string) =
                match System.Decimal.TryParse(str, System.Globalization.NumberStyles.Number, System.Globalization.CultureInfo.InvariantCulture) with
                | (true, dec) -> dec
                | _ -> raise (new System.Exception("toDecimal conversion failed for " + str))

            let fromCapToDecimal (value:string) =
                match value with
                | "-" -> 0m
                | _ ->
                    let numeric = toDecimal (value.Substring(0, value.Length - 1))
                    match value[value.Length - 1] with
                    | 'K' -> numeric * 1000m
                    | 'M' -> numeric * 1000000m
                    | 'B' -> numeric * 1000000000m
                    | 'T' -> numeric * 1000000000000m
                    | _   -> raise (new System.Exception("Cap to decimal conversion failed for " + value))

            let toInt (str:string) =
                match System.Int64.TryParse(str.Replace(",", "")) with
                | (true, v) -> v
                | _ -> raise (new System.Exception("toInt conversion failed for " + str))

            match row.GetAttributeValue("data-row-key", "") with
            | "" -> None
            | ticker ->
                Some {
                    ticker = StockTicker.create ticker
                    company = cellText "company"
                    sector = cellText "sector"
                    industry = cellText "industry"
                    country = cellText "country"
                    marketCap = cellText "marketCap" |> fromCapToDecimal
                    price = cellText "price" |> toDecimal
                    change = (cellText "change").Replace("%", "") |> toDecimal
                    volume = cellText "volume" |> toInt
                }

        let newLayoutRows = doc.DocumentNode.SelectNodes("//div[contains(@class,'fv-table_row') and @data-row-key]")

        // this code is very sensitive to changes on finviz side...
        let nodes = doc.DocumentNode.SelectNodes("//table[@class='styled-table-new is-rounded is-tabular-nums w-full screener_table']/tr")

        match newLayoutRows, nodes with
        | null, null -> Seq.empty
        | null, _ ->
            nodes
                |> Seq.map processScreenerRow
                |> Seq.filter (fun r -> r.IsSome)
                |> Seq.map (fun r -> r.Value)
        | rows, _ ->
            rows
                |> Seq.map processNewScreenerRow
                |> Seq.filter (fun r -> r.IsSome)
                |> Seq.map (fun r -> r.Value)
