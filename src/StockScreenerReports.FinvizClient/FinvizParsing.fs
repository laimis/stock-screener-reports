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
                System.Int32.Parse(str)

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
        let nodes = doc.DocumentNode.SelectNodes("//table[@class='table-light is-new']")

        match nodes with
        | null -> Seq.empty
        | _ -> 
            nodes 
                |> Seq.collect (fun n -> n.ChildNodes)  // this should be all tr nodes
                |> Seq.skip 2                           // skip what looks like text element and a header row
                |> Seq.map processScreenerRow
                |> Seq.filter (fun r -> r.IsSome)
                |> Seq.map (fun r -> r.Value)
