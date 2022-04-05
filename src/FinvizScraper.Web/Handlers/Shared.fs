namespace FinvizScraper.Web.Handlers

module Shared =
    open Giraffe.ViewEngine

    let fullWidthTableAttributes = _class "table is-fullwidth"

    let generateJSForChart title chartCanvasId labels data =

        let formattedLabels = labels |> List.map (fun l -> $"'{l}'") |> String.concat ","
        let formattedData = data |> List.map (fun d -> $"{d}") |> String.concat ","

        printf "%s" formattedData
        printf "%s" formattedLabels

        rawText ("""
            const labels = [
                """ + formattedLabels + """
            ];
            const data = {
                labels: labels,
                datasets: [{
                label: '""" + title + """',
                backgroundColor: 'rgb(255, 99, 132)',
                borderColor: 'rgb(255, 99, 132)',
                data: [""" + formattedData + """],
                }]
            };
            const config = {
                type: 'bar',
                data: data,
                options: {}
            };
            const myChart = new Chart(
                document.getElementById('""" + chartCanvasId + """'),
                config
            );
            """)

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

    let toNameCountTable title listOfNameCountPairs =
        table [fullWidthTableAttributes] (listOfNameCountPairs |> toNameCountRows title)

    let mainLayout pageTitle (content: XmlNode list) =
        html [] [
            head [] [
                title []  [ encodedText pageTitle ]
                
                link [ _rel  "stylesheet"
                       _type "text/css"
                       _href "/main.css" ]

                link [ _rel "stylesheet"
                       _href "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css"]

                script [ _src "https://cdn.jsdelivr.net/npm/chart.js" ] []

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