namespace FinvizScraper.Web.Handlers

module Shared =
    open Giraffe.ViewEngine

    let fullWidthTableAttributes = _class "table is-fullwidth"

    let mainLayout (content: XmlNode list) =
        html [] [
            head [] [
                title []  [ encodedText "FinvizScraper.Web" ]
                link [ _rel  "stylesheet"
                       _type "text/css"
                       _href "/main.css" ]
            ]
            body [] content
        ]