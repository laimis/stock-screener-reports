namespace FinvizScraper.FinvizClient

module FinvizClient =
    open FinvizScraper.Core

    let fetchScreenerHtml (url:string) =
        // make sure that we sleep a bit before each request
        System.Threading.Thread.Sleep(500)
        let web = HtmlAgilityPack.HtmlWeb()
        web.Load(url)

    let parseScreenerHtml (doc:HtmlAgilityPack.HtmlDocument) =

        let nodes = doc.DocumentNode.SelectNodes("//table[@id='screener-views-table']/tr")

        // 3rd node has the tickers

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

        nodes 
            |> skipAndTake 3 1                      // skip three tr nodes, take one that has the tickers table
            |> Seq.collect (fun n -> n.ChildNodes)  // this should contain #text, <td>, #text
            |> skipAndTake 1 1                      // skip one #text, take one that has td
            |> Seq.collect (fun n -> n.ChildNodes)  // this should contain #text <table> #text
            |> skipAndTake 1 1                      // skip one #text, take table
            |> Seq.collect (fun n -> n.ChildNodes)  // this should be all tr nodes
            |> Seq.skip 2                           // skip what looks like text element and a header row
            |> Seq.map processScreenerRow
            |> Seq.filter (fun r -> r.IsSome)
            |> Seq.map (fun r -> r.Value)

    let getResults url =
        let rec fetchPage offset (results:list<ScreenerResult>) =
            let urlToFetch = url + "&r=" + offset.ToString()
            let htmlDoc = fetchScreenerHtml urlToFetch
            
            let page = 
                htmlDoc
                |> parseScreenerHtml
                |> Seq.toList

            // check if the page has only one element and offset is greater than one
            // this potentially could be repeated result, finviz just returns the last result
            // whenever a new page is requested. When total results is something like 20 or 40
            // we don't know that this is the end and just keep fetching
            let isLastPage = page.Length = 1 && offset > 1
            let index = results |> List.tryFindIndex (fun r -> r.ticker.Equals(page.Item(0).ticker))

            if index.IsSome && isLastPage then
                results
            else
                match page with 
                    | c when c.Length = 20 -> fetchPage (offset + 20) (List.append results page)
                    | _ -> (List.append results page)

        fetchPage 1 []