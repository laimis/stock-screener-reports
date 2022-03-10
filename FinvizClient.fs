namespace FinvizScraper

module FinvizClient =

    let fetchScreenerHtml (url:string) =
        let web = HtmlAgilityPack.HtmlWeb()
        web.CachePath <- "cache"
        web.UsingCache <- true

        web.Load(url)

    // // total is within a td cell, starts with Total:
    // // after that read anything up to </td>
    // // trim new lines, whitespace
    // // and convert to int
    // let parseTotalCount (html:string) =
    //     let textTotalIndex = html.IndexOf("Total:")
    //     let endOfTdIndex = html.IndexOf("</td>", textTotalIndex)
    //     let totalSubstring = html.Substring(textTotalIndex + 6,  endOfTdIndex - textTotalIndex - 6)
    //     // at this point totalSubstring is something like this: " </b>4 #1"
    //     let bStartIndex = totalSubstring.IndexOf("</b")
    //     let spaceIndex = totalSubstring.LastIndexOf(" ")
    //     let total = int(totalSubstring.Substring(bStartIndex + 4, spaceIndex - bStartIndex - 4))
    //     total

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

        let processScreenerRow (node:HtmlAgilityPack.HtmlNode) : ScreenerResult =
            
            let tickerNode = extractValueFromScreenerCell node.ChildNodes[2]
            let companyNode = extractValueFromScreenerCell node.ChildNodes[3]
            let sectorNode = extractValueFromScreenerCell node.ChildNodes[4]
            let industryNode = extractValueFromScreenerCell node.ChildNodes[5]
            let countryNode = extractValueFromScreenerCell node.ChildNodes[6]
            let marketCapNode = extractValueFromScreenerCell node.ChildNodes[7]
            let priceNode = extractValueFromScreenerCell node.ChildNodes[9]
            let changeNode = extractValueFromScreenerCell node.ChildNodes[10]
            let volumeNode = extractValueFromScreenerCell node.ChildNodes[11]
            {
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

    let getResults url =
        let rec whileFetch offset results =
            let urlToFetch = url + "&r=" + offset.ToString()
            let htmlDoc = fetchScreenerHtml urlToFetch
            
            let page = 
                htmlDoc
                |> parseScreenerHtml
                |> Seq.toList

            match page with 
                | c when c.Length = 20 -> whileFetch (offset + 20) (List.append results page)
                | _ -> (List.append results page)

        whileFetch 1 []