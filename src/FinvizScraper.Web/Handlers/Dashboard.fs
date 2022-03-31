namespace FinvizScraper.Web.Handlers

module Dashboard =

    open Giraffe.ViewEngine
    open FinvizScraper.Core

    let private layout (content: XmlNode list) =
        html [] [
            head [] [
                title []  [ encodedText "FinvizScraper.Web" ]
                link [ _rel  "stylesheet"
                       _type "text/css"
                       _href "/main.css" ]
            ]
            body [] content
        ]

    let private partial () =
        h1 [] [ encodedText "FinvizScraper.Web" ]

    let private view (screeners:list<Screener>) =
        let rows =
            screeners
            |> List.map (fun screener -> 
                tr [] [
                    td [] [ encodedText screener.name ]
                ])
        let tbl = table [] rows
        
        [tbl] |> layout

    let handler()  = 
        
        // get screeners, render them in HTML
        let screeners = FinvizScraper.Storage.Storage.getScreeners()
        
        let view      = view screeners
        Giraffe.Core.htmlView view