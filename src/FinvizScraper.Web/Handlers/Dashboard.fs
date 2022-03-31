namespace FinvizScraper.Web.Handlers

module Dashboard =

    open Giraffe.ViewEngine
    open FinvizScraper.Core

    let private view (screeners:list<(Screener*int)>) =
        let rows =
            screeners
            |> List.map (fun (screener, count) -> 
                tr [] [
                    td [] [
                        a [ _href $"/screeners/{screener.id}/results" ] [ encodedText screener.name ] ]
                    td [] [ str (count.ToString()) ]
                ])
        let tbl = table [] rows
        
        [tbl] |> Shared.mainLayout

    let handler()  = 
        
        // get screeners, render them in HTML
        let screenerResults = FinvizScraper.Storage.Reports.getLatestScreeners()
        
        let view      = view screenerResults
        Giraffe.Core.htmlView view