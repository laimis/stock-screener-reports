namespace FinvizScraper.Web.Shared

module Charts =
    open Giraffe.ViewEngine.HtmlElements
    open System
    open Giraffe.ViewEngine.Attributes

    type ChartHeight = ChartHeight of string
    type ChartType =
        | Bar
        | Line

    let smallChart = Some (ChartHeight "80")

    let private generateJSForChart
        title
        chartType
        (maxYValue:option<int>)
        chartCanvasId
        labels
        data
        color =

        let formattedLabels = labels |> List.map (fun l -> $"'{l}'") |> String.concat ","
        let formattedData = data |> List.map (fun d -> $"{d}") |> String.concat ","

        let chartTypeString =
            match chartType with
            | Bar -> "bar"
            | Line -> "line"

        let maxYValueString =
            match maxYValue with
            | Some m -> $"max: {m},"
            | None -> ""

        let options = "{
                    scales: {
                        y: {
                            " + maxYValueString + "
                            beginAtZero: true
                        }
                    },
                    plugins: {
                        datalabels : {
                            // color: 'white',
                            anchor: 'end',
                            align: 'end',
                            formatter: function(value, context) {
                                return value > 1000000 ? (value / 1000000).toFixed(0) + 'M' : value;
                            }
                        }
                    }
                }"

        rawText ("""
            const config""" + chartCanvasId + """ = {
                type: '""" + chartTypeString + """',
                data: {
                    labels: [""" + formattedLabels + """],
                    datasets: [{
                        label: '""" + title + """',
                        backgroundColor: '""" + color + """',
                        borderColor: '""" + color + """',
                        data: [""" + formattedData + """],
                    }]
                },
                plugins: [ChartDataLabels],
                options: """ + options + """
            };
            const myChart""" + chartCanvasId + """ = new Chart(
                document.getElementById('""" + chartCanvasId + """'),
                config""" + chartCanvasId + """
            );
            """)

    
    let private generateChartElements
        title
        chartType
        (maxYValue:option<int>)
        (height:option<ChartHeight>)
        labels
        data
        color =
        let chartGuid = Guid.NewGuid().ToString("N")
        let canvasId = $"chart{chartGuid}"

        let standardAttributes =[
            _id canvasId
            attr "aria-label" $"Chart for {title}"
            attr "role" "img"
        ] 
        
        let attributes = 
            match height with
            | Some (ChartHeight h) -> (attr "height" h)::standardAttributes
            | None -> standardAttributes

        let chartDiv = div [] [
            canvas attributes [
                p [] [ str $"Chart for {title}" ]
            ]
        ]

        let chartScript = script [_type "application/javascript"] [
            generateJSForChart title chartType maxYValue canvasId labels data color
        ]

        [
            chartDiv
            chartScript
        ]

    let convertNameCountsToChart
        title
        chartType
        maxYValue
        height
        color
        listOfNameCountPairs =
        let labels = listOfNameCountPairs |> List.map (fun ((name:DateTime),_) -> name.ToString("MMM/dd"))
        let data = listOfNameCountPairs |> List.map (fun (_,count) -> count)

        generateChartElements title chartType maxYValue height labels data color