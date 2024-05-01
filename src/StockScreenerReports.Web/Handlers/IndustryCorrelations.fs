namespace StockScreenerReports.Web.Handlers

open System.Web
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open StockScreenerReports.Web.Shared

module IndustryCorrelations =
    open Giraffe
    open Giraffe.ViewEngine
    open StockScreenerReports.Core
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared.Views
    
    let private generateHeader industries (selectedIndustry: string option) additionalFields startDate endDate =
        let title = "Industry Correlations"
        let onChangeAttribute = KeyValue("onchange", "location.href = '?industry=' + encodeURIComponent(this.value)")
        let industryDropdown =
            div [ _class "select" ] [
                select [
                    onChangeAttribute
                ] [
                    option [ _value "" ] [ str "Correlation Summary" ]
                    for industry in industries do
                        option [
                            _value industry
                            if selectedIndustry.IsSome && selectedIndustry.Value = industry then _selected
                        ] [ str industry ]
                ]
            ]
            
        div [_class "columns"] [
            div [_class "column is-one-third"] [
                h4 [] [ str title ]
                industryDropdown
            ]
            div [_class "column has-text-right"] [ generateFilterSection additionalFields (startDate, endDate)]
        ]
        
    let private generateCorrelationTable industries (selectedIndustry:string) (correlationMatrix:Matrix<float>) =
        let correlationTableHeaders = [ "Industry"; "Correlation"; "" ]

        let industryIndex = industries |> List.findIndex ((=) selectedIndustry)
        let correlations = correlationMatrix.Row(industryIndex).ToArray()
            
        let correlationRows =
            industries
            |> List.mapi (fun i ind -> (ind, correlations[i]))
            |> List.filter (fun (ind, _) -> ind <> selectedIndustry)
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
        
        fullWidthTableWithSortableHeaderCells correlationTableHeaders correlationRows
        
    let handler: HttpHandler =
        fun (next: HttpFunc) (ctx: Microsoft.AspNetCore.Http.HttpContext) ->
            
            // param processing
            let startDate, endDate = getFilterSectionParams ctx
            let industryParam = ctx.GetQueryStringValue("industry")
            let selectedIndustry, additionalFields =
                match industryParam with
                | Ok value ->
                    match value with
                    | x when System.String.IsNullOrWhiteSpace(x) -> None, []
                    | _ -> Some value, ["industry", value]
                | Error _ -> None, []
            
            let industries = Storage.getIndustries()
            
            let industrySMABreakdowns =
                industries
                |> List.map (fun industry ->
                    let breakdowns = Reports.getIndustrySMABreakdownsForDateRange SMA.SMA20 (startDate, endDate) industry
                    breakdowns |> List.map (fun breakdown -> breakdown.breakdown.percentAbove |> float)
                )
                |> List.map List.toArray
                |> List.toArray

            let correlationMatrix = industrySMABreakdowns |> Correlation.PearsonMatrix
            
            let contentToRender =
                match selectedIndustry with
                | Some selectedIndustry -> correlationMatrix |> generateCorrelationTable industries selectedIndustry
                | None -> div [] []
            
            let header = generateHeader industries selectedIndustry additionalFields startDate endDate

            let view =
                div [ _class "content" ] [
                    header
                    contentToRender
                ]

            ([ view ] |> mainLayout "Industry Correlations") next ctx