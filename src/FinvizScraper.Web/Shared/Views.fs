namespace FinvizScraper.Web.Shared

module Views =
    open Giraffe.ViewEngine
    open FinvizScraper.Web.Shared

    let billion = 1_000_000_000m
    let million = 1_000_000m

    let marketCapFormatted marketCap =
        match marketCap with
        | d when d > billion -> System.Math.Round(d / billion, 2).ToString() + " B"
        | d -> System.Math.Round(d / million, 2).ToString() + "M"

    let dollarFormatted (value:decimal) =
        value.ToString("C")

    let percentFormatted (value:decimal) =
        $"{value} %%"

    let volumeFormatted (value:int) =
        value.ToString("N0")

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

    let generateHrefWithElement element link =
        a [
            _href link
            _target "_blank"
        ] [
            element
        ]

    let generateHrefWithAttr title link attr =
        a [
            _href link
            _target "_blank"
            attr
        ] [
            str title
        ]

    let private toNameCountRows breakdownName nameElementFunc list =
        let rows =
            list
            |> List.truncate 10
            |> List.map (fun (name,count) ->
                tr [] [
                    td [] [ (nameElementFunc name) ]
                    td [] [ str (count.ToString()) ]
                ])

        let header = tr [] [
            th [ _colspan "2"] [ str breakdownName ]
        ]

        header::rows

    let fullWidthTable rows =
        table [ _class "table is-fullwidth" ] rows

    let toNameCountTable title listOfNameCountPairs =
        listOfNameCountPairs |> toNameCountRows title (fun name -> str name) |> fullWidthTable

    let toNameCountTableWithLinks title linkFunction listOfNameCountPairs =
        listOfNameCountPairs |> toNameCountRows title (fun name -> generateHref name (linkFunction name)) |> fullWidthTable

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