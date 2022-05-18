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
        ] [
            str title
        ]

    let generateHrefWithElement element link =
        a [
            _href link
        ] [
            element
        ]

    let generateHrefWithAttr title link attr =
        a [
            _href link
            attr
        ] [
            str title
        ]

    let private toNameCountRows breakdownName maxNumberOfRows nameElementFunc list =
        let rows =
            list
            |> List.truncate maxNumberOfRows
            |> List.map (fun (name,count) ->
                tr [] [
                    td [] [ (nameElementFunc name) ]
                    td [ _class "has-text-right"] [ str (count.ToString()) ]
                ])

        let header = tr [] [
            th [ _colspan "2"] [ str breakdownName ]
        ]

        header::rows

    let fullWidthTable rows =
        table [ _class "table is-fullwidth" ] rows

    let toNameCountTable title maxNumberOfRows listOfNameCountPairs =
        listOfNameCountPairs |> toNameCountRows title maxNumberOfRows (fun name -> str name) |> fullWidthTable

    let toNameCountTableWithLinks title maxNumberOfRows linkFunction listOfNameCountPairs =
        listOfNameCountPairs |> toNameCountRows title maxNumberOfRows (fun name -> generateHref name (linkFunction name)) |> fullWidthTable

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

    let generateScreenerTags idAndNameTuple =
        let (id,name) = idAndNameTuple

        // TODO: screemer id mapping is hardcoded here
        let backgroundColor =
            match id with
            | 27 -> "#2C59D8" // new high
            | 28 -> "#3590F3" // new high (w/ sales)
            | 29 -> "#4DBEF7" // top gainer
            | 31 -> "#90323C" // new low
            | 30 -> "#C54A8B" // top loser
            | _ -> raise (new System.Exception($"Unknown screener id {id} for screener tags")) // otherwise blow up

        div [ _class "tags has-addons" ] [
            span [ 
                _class $"tag "
                _style $"background-color: {backgroundColor};"
            ] [
                "" |> str
            ]
            span [ _class "tag" ] [
                name |> str
            ]
        ]

    let genericJobStatusGet jobName =
            match (FinvizScraper.Storage.Storage.getLatestJobStatus jobName) with
                | Some (message, timestamp) -> $"{message} @ {timestamp}"
                | None -> $"No results found for {jobName} found"