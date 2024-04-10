namespace StockScreenerReports.Web.Shared

module Charts =
    open Giraffe.ViewEngine.HtmlElements
    open System
    open Giraffe.ViewEngine.Attributes

    type ChartHeight = ChartHeight of string
    type ChartType =
        | Bar
        | Line

    let smallChart = Some (ChartHeight "80")

    type DataSet<'a> =
        {
            data: list<'a>
            title: string
            color: string
        }

    let private generateJSForChart
        chartType
        (maxYValue:option<int>)
        chartCanvasId
        (labels:seq<string>)
        (datasets:list<DataSet<'a>>)
        =

        let chartTypeString =
            match chartType with
            | Bar -> "bar"
            | Line -> "line"

        let maxYValueString =
            match maxYValue with
            | Some m -> $"max: {m},"
            | None -> ""

        let options = "{
                    // responsive: true,
                    maintainAspectRatio: false,
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

        let datasetsString = 
            datasets
            |> List.map ( fun dataset -> 
                let formattedData = 
                    dataset.data
                    |> List.map (fun d ->
                        let str = $"{d}"
                        // both return the same value, I am experimenting with
                        // how it looks if empty string is returned on zero
                        match str with
                        | "0" -> str
                        | _ -> str
                    )
                    |> String.concat ","

                """{
                    label: '""" + dataset.title + """',
                    backgroundColor: '""" + dataset.color + """',
                    borderColor: '""" + dataset.color + """',
                    tension: 0.1,
                    data: [""" + formattedData + """],
                }"""
            )
            |> String.concat ","

        let formattedLabels = labels |> Seq.map (fun l -> $"'{l}'") |> String.concat ","

        rawText ("""
            const config""" + chartCanvasId + """ = {
                type: '""" + chartTypeString + """',
                data: {
                    labels: [""" + formattedLabels + """],
                    datasets: [""" + datasetsString + """]
                },
                plugins: [ChartDataLabels],
                options: """ + options + """
            };
            const myChart""" + chartCanvasId + """ = new Chart(
                document.getElementById('""" + chartCanvasId + """'),
                config""" + chartCanvasId + """
            );
            """)

    
    let generateChartElements
        title
        chartType
        (maxYValue:option<int>)
        (height:option<ChartHeight>)
        (labels:seq<string>)
        datasets =

        let chartGuid = Guid.NewGuid().ToString("N")
        let canvasId = $"chart{chartGuid}"

        let standardAttributes =[
            _id canvasId
            attr "aria-label" $"Chart for {title}"
            attr "role" "img"
            attr "style" "width: 100%; height: 300px"
        ] 
        
        let attributes = 
            match height with
            | Some (ChartHeight h) -> standardAttributes
            | None -> standardAttributes

        let chartDiv = div [] [
            canvas attributes [
                p [] [ str $"Chart for {title}" ]
            ]
        ]

        let chartScript = script [_type "application/javascript"] [
            generateJSForChart chartType maxYValue canvasId labels datasets
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

        let labels = listOfNameCountPairs |> Seq.map (fun (name:DateTime,_) -> name.ToString("yyyy-MM-dd"))
        let data = listOfNameCountPairs |> Seq.map snd |> Seq.toList

        let datasets = [
            {
                data = data
                title = title
                color = color
            }
        ]

        generateChartElements title chartType maxYValue height labels datasets