namespace StockScreenerReports.FinvizClient

module FinvizParsing =
    open StockScreenerReports.Core

    let mutable outputFunc = (fun _ -> ())

    let setOutputFunc (f:string -> unit) =
        outputFunc <- f

    let parseResultCount (doc:HtmlAgilityPack.HtmlDocument) =
        let nodes =
            doc.DocumentNode.SelectNodes("//table[@id='screener-views-table']/tr")
            |> Seq.toList

        let nodesContainingTotal = nodes.Item(2).SelectNodes("//div[@id='screener-total']")

        let totalText =
            match nodesContainingTotal with  // seems like the div above is not always present, sometimes it has a table with rows indicating total
            | null -> nodes.Item(2).SelectNodes("//td[@class='count-text']").Item(0).InnerText
            | _ -> nodesContainingTotal.Item(0).InnerText

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

            // Converts market cap string to decimal value
            // Supported formats (all tested and validated):
            //   "-"     -> 0 (unknown market cap)
            //   "1.75"  -> 1,750,000 (plain number treated as millions)
            //   "100M"  -> 100,000,000 (M = millions)
            //   "5.5B"  -> 5,500,000,000 (B = billions)
            //   "500K"  -> 500,000 (K = thousands)
            // 
            // Note: Plain numbers without suffix are interpreted as millions. This convention
            // was established to handle cases where Finviz returns values like "1.75" without
            // a suffix. The millions interpretation (treating "1.75" as $1.75M) is consistent 
            // with common financial conventions where the 'M' suffix is implicit for smaller values.
            // This interpretation was added to fix the error: "Cap to decimal conversion failed for 1.75"
            let fromCapToDecimal (value:string) =
                match value with
                | "-" -> 0m // sometimes when it does know know cap, it returns -
                | _ ->
                    let lastChar = value[value.Length - 1]
                    
                    // Check if last character is a letter (suffix) or digit (plain number)
                    if System.Char.IsDigit(lastChar) then
                        // Plain number without suffix - treat as millions (implicit M)
                        // Handles cases where Finviz returns values like "1.75"
                        match value with
                        | Decimal dec -> dec * 1000000m
                        | _ -> raise (new System.Exception("fromCap plain number conversion failed for " + value))
                    else
                        // Has a suffix character
                        let numericPortion = 
                            match value.Substring(0, value.Length - 1) with
                            | Decimal dec -> dec
                            | _ -> raise (new System.Exception("fromCap numeric conversion failed for " + value))

                        match lastChar with
                        | 'M' -> numericPortion * 1000000m
                        | 'B' -> numericPortion * 1000000000m
                        | 'K' -> numericPortion * 1000m  // Tested: "500K" -> 500,000
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

        // this code is very sensitive to changes on finviz side...
        let nodes = doc.DocumentNode.SelectNodes("//table[@class='styled-table-new is-rounded is-tabular-nums w-full screener_table']/tr")

        match nodes with
        | null -> Seq.empty
        | _ -> 
            nodes 
                |> Seq.map processScreenerRow
                |> Seq.filter (fun r -> r.IsSome)
                |> Seq.map (fun r -> r.Value)
