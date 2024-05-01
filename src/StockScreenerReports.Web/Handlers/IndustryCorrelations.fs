namespace StockScreenerReports.Web.Handlers

open System.Web
open MathNet.Numerics.Statistics
open StockScreenerReports.Web.Shared

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
                | Ok value ->
                    match value with
                    | x when System.String.IsNullOrWhiteSpace(x) -> None
                    | _ -> Some value
                | Error _ -> None

            let correlationTableHeaders = [ "Industry"; "Correlation"; "" ]

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
                        let heatClass =
                            match corr with
                            | x when x > 0.85 -> "has-background-success"
                            | x when x > 0.8 -> "has-background-success-light"
                            | x when x < 0 -> "has-background-warning"
                            | _ -> ""
                            
                        tr [_class heatClass] [
                            LinkColumn(ind, "?industry=" + HttpUtility.UrlEncode(ind)) |> toTd
                            [corr |> decimal |> NumberColumn |> toNode] |> td []
                            LinkNewTabColumn("Industry Page", ind |> Links.industryLink) |> toTd
                        ]
                    )
                | None -> []

            let correlationTable = fullWidthTableWithSortableHeaderCells correlationTableHeaders correlationRows

            let onChangeAttribute = KeyValue("onchange", "location.href = '?industry=' + encodeURIComponent(this.value)")
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