namespace StockScreenerReports.Web.Handlers

open MathNet.Numerics.Statistics

module IndustryCorrelations =
    open Giraffe
    open Giraffe.ViewEngine
    open StockScreenerReports.Core
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared.Views
    
    let handler: HttpHandler =
        fun (next: HttpFunc) (ctx: Microsoft.AspNetCore.Http.HttpContext) ->
            let industries = Storage.getIndustries()

            let industrySMABreakdowns =
                industries
                |> List.map (fun industry ->
                    let breakdowns = Reports.getIndustrySMABreakdownsForDateRange SMA.SMA20 (ReportsConfig.dateRangeAsStrings()) industry
                    breakdowns |> List.map (fun breakdown -> breakdown.breakdown.percentAbove |> float)
                )
                |> List.map List.toArray
                |> List.toArray

            let correlationMatrix = Correlation.PearsonMatrix(industrySMABreakdowns)

            let industryParam = ctx.GetQueryStringValue("industry")
            let selectedIndustry =
                match industryParam with
                | Ok value -> Some value
                | Error _ -> None

            let correlationTableHeaders = [ "Industry"; "Correlation" ]

            let correlationRows =
                match selectedIndustry with
                | Some industry ->
                    let industryIndex = industries |> List.findIndex ((=) industry)
                    let correlations = correlationMatrix.Row(industryIndex).ToArray()
                    industries
                    |> List.mapi (fun i ind -> (ind, correlations.[i]))
                    |> List.filter (fun (ind, _) -> ind <> industry)
                    |> List.sortByDescending snd
                    |> List.map (fun (ind, corr) ->
                        tr [] [
                            StringColumn(ind) |> toTd
                            corr |> decimal |> NumberColumn |> toTd
                        ]
                    )
                | None -> []

            let correlationTable = fullWidthTableWithSortableHeaderCells correlationTableHeaders correlationRows

            let onChangeAttribute = KeyValue("onchange", "location.href = '?industry=' + this.value")
            let industryDropdown =
                div [ _class "select" ] [
                    select [
                        onChangeAttribute
                    ] [
                        option [ _value "" ] [ str "Select an industry" ]
                        for industry in industries do
                            option [
                                _value industry
                                if selectedIndustry.IsSome && selectedIndustry.Value = industry then _selected
                            ] [ str industry ]
                    ]
                ]

            let title = "Industry Correlations"
            let view =
                toSection title (
                    div [ _class "content" ] [
                        industryDropdown
                        if selectedIndustry.IsSome then
                            correlationTable
                    ]
                )

            ([ view ] |> mainLayout "Industry Correlations") next ctx