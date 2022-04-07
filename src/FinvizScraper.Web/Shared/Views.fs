namespace FinvizScraper.Web.Shared

module Views =
    open Giraffe.ViewEngine
    open System
    open FinvizScraper.Web.Shared

    let generateTickerLink ticker =
        a [
            ticker |> Links.stockLink |> _href
        ] [
            str ticker
        ]

    let generateHref title link =
        a [
            _href link
            _target "_blank"
        ] [
            str title
        ]

    let generateHrefWithAttr title link attr =
        a [
            _href link
            _target "_blank"
            attr
        ] [
            str title
        ]

    let private toNameCountRows breakdownName list =
        let rows =
            list
            |> List.truncate 10
            |> List.map (fun (name,count) ->
                tr [] [
                    td [] [ str name ]
                    td [] [ str (count.ToString()) ]
                ])

        let header = tr [] [
            th [ _colspan "2"] [ str breakdownName ]
        ]

        header::rows

    let fullWidthTable rows =
        table [ _class "table is-fullwidth" ] rows

    let toNameCountTable title listOfNameCountPairs =
        listOfNameCountPairs |> toNameCountRows title |> fullWidthTable

    let mainLayout pageTitle (content: XmlNode list) =
        html [] [
            head [] [
                title []  [ encodedText pageTitle ]

                link [
                    _rel "stylesheet"
                    Links.bulmaCssLink |> _href
                ]

                script [ Links.chartJsLink |> _src ] []
                script [ Links.chartJsDatalabelsLink |> _src ] []

                meta [
                    _name "viewport"
                    _content "width=device-width, initial-scale=1"
                ]
            ]
            body [] [
                section [_class "section"] [
                    div [_class "container"] content
                ]
            ]
        ] |> Giraffe.Core.htmlView
    
    let notFound message =
        let view = 
            div [_class "content"] [
                h1 [] [
                    str message
                ]
            ]
        
        [view] |> mainLayout message