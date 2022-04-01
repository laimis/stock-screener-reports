namespace FinvizScraper.Web.Handlers

module Shared =
    open Giraffe.ViewEngine

    let fullWidthTableAttributes = _class "table is-fullwidth"

    let mainLayout pageTitle (content: XmlNode list) =
        html [] [
            head [] [
                title []  [ encodedText pageTitle ]
                link [ _rel  "stylesheet"
                       _type "text/css"
                       _href "/main.css" ]
                link [ _rel "stylesheet"
                       _href "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css"]
            ]
            body [] content
        ]