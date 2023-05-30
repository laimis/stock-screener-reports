namespace StockScreenerReports.Web.Handlers

module Cycles =
    open Giraffe
    open StockScreenerReports.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared.Views
    open Giraffe.ViewEngine
    open StockScreenerReports.Core
    

    let internal generateIndustryCycleStartChart (cycles:IndustryWithCycle list) =

        let cyclesGroupedByDate =
            cycles
            |> List.groupBy (fun (_, x) -> x.startPointDateFormatted)
            |> Map.ofList

        let startPointDateSelector = fun (_, x) -> x.startPoint.date
        let minStart = cycles |> List.minBy startPointDateSelector |> startPointDateSelector
        let maxStart = cycles |> List.maxBy startPointDateSelector |> startPointDateSelector

        let dateCounts = 
            ReportsConfig.listOfBusinessDates (minStart, maxStart)
            |> Seq.map (fun date -> 
                let dateFormatted = date.ToString("d")
                let cyclesForDate = cyclesGroupedByDate |> Map.tryFind dateFormatted
                match cyclesForDate with
                | Some cycles -> (date, decimal cycles.Length)
                | None -> (date, 0m)
            )

        let dataset:Charts.DataSet<decimal> =
            {
                data = dateCounts |> Seq.map snd |> List.ofSeq
                title = $"start counts"
                color = Constants.ColorRed
            }

        let maxValue = (dateCounts |> Seq.map snd |> Seq.max) + 5m |> int

        let labels = dateCounts |> Seq.map (fun (date,_) -> date.ToString("MM/dd"))
        let chart = [dataset] |> Charts.generateChartElements "Start counts" Charts.ChartType.Bar (Some maxValue) Charts.smallChart labels

        section [ _class "content" ] [
            h4 [] [ str "Industry Cycle Start Counts" ]
            div [] chart
        ]
            
    type private ColumnValue =
            | Link of string * string
            | String of string
            | Date of System.DateTime
            | Number of decimal
            
    let private generateCyclesTable (cycles:list<IndustryWithCycle>) =
        
        let rows =
            cycles
            |> List.map (fun (industry, cycle) ->
                [
                    Link(industry,industry |> Links.industryLink)
                    Date(cycle.startPointDate)
                    Number(cycle.startPointValue)
                    Date(cycle.highPointDate)
                    Number(cycle.highPointValue)
                    Number(cycle.currentPointValue)
                    String(cycle.age.TotalDays |> int |> string)
                    String(cycle.highPointAge.TotalDays |> int |> string)
                ]
            )
            |> List.map (fun data ->
                tr [] (
                    data
                    |> List.map (fun cell ->
                        td [] [ 
                            match cell with
                            | Link (text, link) -> generateHref text link
                            | String text -> str text
                            | Date date -> date.ToString("yyyy-MM-dd") |> str
                            | Number number -> number.ToString("N2") |> str
                        ]
                    )
                )
            )

        let header = [
            "Industry"
            "Start"
            "Start Value"
            "High"
            "High Value"
            "Current Value"
            "Age"
            "High Age"
        ]

        rows |> fullWidthTableWithSortableHeaderCells header

    let handler : HttpHandler  =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->

            let cycles =
                Constants.SMA20
                |> Storage.getIndustryCycles
            
            let industryCycleSection = 
                cycles
                |> generateIndustryCycleStartChart

            let cycleTableSection =
                cycles
                |> generateCyclesTable

            let view = [
                industryCycleSection
                cycleTableSection
            ]
            
            (view |> mainLayout $"Cycles") next ctx