namespace FinvizScraper.Web.Handlers

module Dashboard =

    open Giraffe.ViewEngine

    let private view (screeners:list<(FinvizScraper.Storage.Reports.ScreenerResultReport)>) =
        let rows =
            screeners
            |> List.map (fun screener -> 
                let screenerDate = screener.date.ToString("yyyy-MM-dd")
                tr [] [
                    td [] [
                        a [ _href $"/screeners/{screener.screenerid}/results/{screenerDate}" ] [ encodedText screener.name ] ]
                    td [] [ str (screener.count.ToString()) ]
                ])
        let tbl = table [Shared.fullWidthTableAttributes] rows
        
        [tbl] |> Shared.mainLayout "Dashboard"

    let handler()  = 
        
        // get screeners, render them in HTML
        let screenerResults = FinvizScraper.Storage.Reports.getLatestScreeners()
        
        let view      = view screenerResults
        Giraffe.Core.htmlView view