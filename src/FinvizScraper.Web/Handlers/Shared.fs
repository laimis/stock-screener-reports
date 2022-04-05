namespace FinvizScraper.Web.Handlers

module Shared =
    open Giraffe.ViewEngine
    open System

    let fullWidthTableAttributes = _class "table is-fullwidth"

    let generateTickerLink ticker =
        a [
            _href $"/stocks/{ticker}"
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

    let generateJSForChart title chartCanvasId labels data =

        let formattedLabels = labels |> List.map (fun l -> $"'{l}'") |> String.concat ","
        let formattedData = data |> List.map (fun d -> $"{d}") |> String.concat ","

        rawText ("""
            const config""" + chartCanvasId + """ = {
                type: 'bar',
                data: {
                    labels: [""" + formattedLabels + """],
                    datasets: [{
                        label: '""" + title + """',
                        backgroundColor: 'rgb(255, 99, 132)',
                        borderColor: 'rgb(255, 99, 132)',
                        data: [""" + formattedData + """],
                    }]
                },
                options: {}
            };
            const myChart""" + chartCanvasId + """ = new Chart(
                document.getElementById('""" + chartCanvasId + """'),
                config""" + chartCanvasId + """
            );
            """)

    
    let generateChartElements title labels data =
        let chartGuid = Guid.NewGuid().ToString("N")
        let canvasId = $"chart{chartGuid}"

        let chartDiv = div [] [
            canvas [ _id canvasId ] []
        ]

        let chartScript = script [_type "application/javascript"] [
            generateJSForChart title canvasId labels data
        ]

        [
            chartDiv
            chartScript
        ]

    let convertNameCountsToChart title listOfNameCountPairs =
        let labels = listOfNameCountPairs |> List.map (fun ((name:DateTime),_) -> name.ToString("MMM/dd"))
        let data = listOfNameCountPairs |> List.map (fun (_,count) -> count)

        generateChartElements title labels data

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