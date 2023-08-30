namespace StockScreenerReports.Web.Handlers

module Cycles =
    open Giraffe
    open StockScreenerReports.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared.Views
    open Giraffe.ViewEngine
    open StockScreenerReports.Core
    
    [<Literal>]
    let private maximumAgeParam = "maximumAge"
    [<Literal>]
    let private minimumValueParam = "minimumValue"
    [<Literal>]
    let private minimumChangeParam = "minimumChange"


    let internal generateIndustryCycleStartChart (cycles:IndustryWithCycle list) =

        match cycles with
        | [] -> 
            toSectionWithNoContent "No cycles found" 
        | _ ->
            let cyclesGroupedByDate =
                cycles
                |> List.groupBy (fun (_, x) -> x.startPointDateFormatted)
                |> Map.ofList

            let startPointDateSelector = fun (_, x) -> x.startPoint.date
            let minStart = cycles |> List.minBy startPointDateSelector |> startPointDateSelector
            let maxStart = ReportsConfig.now().Date

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

            div [] chart |> toSection "Industry Cycle Start Counts"
    
    let private generateFilterSectionForm maximumAge minimumValue minimumChange =

        form [] [
            div [ _class "columns"] [
                div [ _class "field column" ] [
                    label [ _for maximumAgeParam ] [ str "Maximum Age" ]
                    input [
                        _type "number"
                        _name maximumAgeParam
                        _class "input"
                        _value (
                            match maximumAge with
                            | System.Int32.MaxValue -> ""
                            | _ -> maximumAge.ToString()
                        )
                    ]
                ]
                div [ _class "field column" ] [
                    label [ _for minimumValueParam ] [ str "Minimum Value" ]
                    input [
                        _type "number"
                        _name minimumValueParam
                        _class "input"
                        _value (minimumValue.ToString())
                    ]
                ]
                div [ _class "field column" ] [
                    label [ _for minimumChangeParam ] [ str "Minimum Change" ]
                    input [
                        _type "number"
                        _name minimumChangeParam
                        _class "input"
                        _value (minimumChange.ToString())
                    ]
                ]
            ]
            div [ ] [
                input [
                    _type "submit"
                    _class "button is-primary is-small mb-2"
                    _value "Apply Filters"
                ]
                span [] [ str " Quick Filters: " ]
                a [ _href $"/cycles?{maximumAgeParam}=10&{minimumValueParam}=50" ] [ str "max 10 days, minimum 50" ]
                str " | "
                a [ _href $"/cycles?{maximumAgeParam}=30&{minimumValueParam}=50" ] [ str "max 30 days, minimum 50" ]
            ]
        ]

    let private generateCyclesSection maximumAge minimumValue minimumChange (cycles:list<IndustryWithCycle>) =
        
        let filterSection = generateFilterSectionForm maximumAge minimumValue minimumChange

        let rows =
            cycles
            |> List.sortByDescending (fun (_, cycle) -> cycle.change)
            |> List.map (fun (industry, cycle) ->
                let direction =
                    match cycle.change with
                    | x when x <= 0m -> Down
                    | _ -> Up

                let age = int cycle.age.TotalDays
                let change = cycle.change

                let score = 
                    (MarketCycleScoring.calculateScoreComponents direction age change)
                    |> MarketCycleScoring.componentScoreAdding
                    |> decimal

                [
                    LinkNewTabColumn(industry,industry |> Links.industryLink)
                    DateColumn(cycle.startPointDate)
                    StringColumn(cycle.age.TotalDays |> int |> string)
                    NumberColumn(cycle.startPointValue)
                    NumberColumn(cycle.currentPointValue)
                    NumberColumn(cycle.change)
                    DateColumn(cycle.highPointDate)
                    NumberColumn(cycle.highPointValue)
                    StringColumn(cycle.highPointAge.TotalDays |> int |> string)
                    NumberColumn(score)
                ]
            )
            |> List.map toTr

        let header = [
            "Industry"
            "Start"
            "Age"
            "Start Value"
            "Value"
            "Change"
            "High"
            "High Value"
            "High Age"
            "Score"
        ]

        let table = rows |> fullWidthTableWithSortableHeaderCells header
 
        let title = 
            match maximumAge with
            | System.Int32.MaxValue -> "Industry Cycles"
            | _ -> $"Industry Cycles (Maximum Age: {maximumAge} days, Minimum Value: {minimumValue}): {cycles.Length}, Minimum Change: {minimumChange}"

        div [] [
            filterSection
            table
        ] |> toSection title

    let getQueryParams (ctx:Microsoft.AspNetCore.Http.HttpContext) =

        let parseParam paramName parseFunc defaultValue =
            ctx.Request.Query.[paramName]
            |> Seq.tryHead
            |> Option.map (fun x -> 
                match parseFunc(x) with
                | true, value -> value
                | _ -> defaultValue)
            |> Option.defaultValue defaultValue

        let maximumAge = parseParam maximumAgeParam System.Int32.TryParse System.Int32.MaxValue
        let minimumValue = parseParam minimumValueParam System.Decimal.TryParse 0m
        let minimumChange = parseParam minimumChangeParam System.Decimal.TryParse 0m

        (maximumAge, minimumValue, minimumChange)

    let handler : HttpHandler  =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->

            let (maximumAge, minimumValue, minimumChange) = getQueryParams ctx
            let cycleFilterFunc = fun (_, cycle:MarketCycle) -> 
                cycle.age.TotalDays <= maximumAge &&
                cycle.currentPointValue >= minimumValue &&
                cycle.currentPointValue - cycle.startPointValue >= minimumChange

            let cycles =
                Constants.SMA20
                |> Storage.getIndustryCycles

            // get latest job runs 
            let missedJobs =
                Storage.getJobs()
                |> List.filter (fun job -> job.name = TrendsJob)
                |> List.filter Utils.failedJobFilter

            let warningSection = jobAlertSection missedJobs
            
            let industryCycleSection = 
                cycles
                |> generateIndustryCycleStartChart

            let filteredCycles = 
                cycles
                |> List.filter cycleFilterFunc

            let cycleTableSection =
                filteredCycles
                |> generateCyclesSection maximumAge minimumValue minimumChange

            let view = [
                warningSection
                industryCycleSection
                cycleTableSection
            ]
            
            (view |> mainLayout $"Cycles") next ctx