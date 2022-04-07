namespace FinvizScraper.Web.Shared

module Charts =
    open Giraffe.ViewEngine.HtmlElements
    open System
    open Giraffe.ViewEngine.Attributes

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
                plugins: [ChartDataLabels],
                options: {
                    plugins: {
                        datalabels : {
                            // color: 'white',
                            anchor: 'end',
                            align: 'end'
                        }
                    }
                }
            };
            const myChart""" + chartCanvasId + """ = new Chart(
                document.getElementById('""" + chartCanvasId + """'),
                config""" + chartCanvasId + """
            );
            """)

    
    let generateChartElements title height labels data =
        let chartGuid = Guid.NewGuid().ToString("N")
        let canvasId = $"chart{chartGuid}"

        let standardAttributes =[
            _id canvasId
            attr "aria-label" $"Chart for {title}"
            attr "role" "img"
        ] 
        
        let attributes = 
            match height with
            | Some h -> (attr "height" h)::standardAttributes
            | None -> standardAttributes

        let chartDiv = div [] [
            canvas attributes [
                p [] [ str $"Chart for {title}" ]
            ]
        ]

        let chartScript = script [_type "application/javascript"] [
            generateJSForChart title canvasId labels data
        ]

        [
            chartDiv
            chartScript
        ]

    let convertNameCountsToChart title height listOfNameCountPairs =
        let labels = listOfNameCountPairs |> List.map (fun ((name:DateTime),_) -> name.ToString("MMM/dd"))
        let data = listOfNameCountPairs |> List.map (fun (_,count) -> count)

        generateChartElements title height labels data