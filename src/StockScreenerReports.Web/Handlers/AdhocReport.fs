namespace StockScreenerReports.Web.Handlers

module AdhocReport =
    open StockScreenerReports.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open Giraffe.ViewEngine.Attributes
    open StockScreenerReports.Storage
    open FSharp.Data
    open Giraffe

    type AdhocExportType = CsvProvider<
        Schema = "ticker, date  (string), earnings (bool)",
        HasHeaders=false>
    
    let header = "ticker,date"

    let getData() = 
        // all earnings
        let earningsList = Reports.getAllEarningsTickers()

        let earningsSet =
            earningsList
            |> List.map (fun (ticker, date) -> ticker + date.ToString())
            |> Set.ofList

        let combos = Reports.getScreenerResultCombos StockScreenerReports.Core.Constants.NewHighsScreenerId StockScreenerReports.Core.Constants.TopGainerScreenerId

        let mutable counter = 0

        combos
        |> List.map (fun (ticker,date) ->
            let hasEarnings = Set.contains (ticker + date.ToString()) earningsSet
            counter <- counter + 1
            (counter,hasEarnings,ticker,date)
        )

    let exportHandler() =
        setHttpHeader "Content-Type" "text/csv"
        >=> 
            let filename = "adhocexport.csv"
            let escapedFilename = System.Uri.EscapeDataString(filename)
            setHttpHeader "Content-Disposition" $"attachment; filename={escapedFilename}"
        >=>
            let data = getData()
            let rows = 
                data 
                |> List.map (fun (_,hasEarnings,ticker,date) -> 
                    AdhocExportType.Row(
                        ticker,
                        date.ToString("yyyy-MM-dd"),
                        hasEarnings
                    )
                )

            let csv = new AdhocExportType(rows)

            setBodyFromString (header + System.Environment.NewLine + csv.SaveToString())

    let handler()  =
        
        let header = div [_class "content"] [
            h1 [] [str "New High/Top Gainer Combo"]
            div [] [
                form [
                    _method "POST"
                    _action (Links.adhocReportExport)
                ] [
                    button [_class "button is-primary"] [str "Export"]
                ]
            ]
        ]

        let data = getData()

        let rows =
            data
            |> List.map (fun (index,hasEarnings,ticker,date) ->
                
                let earningsIcon = hasEarnings |> Views.generateEarningsIcon
                
                tr [] [
                    td [] [index.ToString() |> str]
                    ticker |> Links.tradingViewLink |> Views.generateHref ticker |> Views.toTdWithNode
                    date |> StockScreenerReports.Core.Utils.convertToDateString |> Views.toTd
                    earningsIcon |> Views.toTdWithNode
                ]
            )

        let table = rows |> Views.fullWidthTable
        
        [header; table] |> Views.mainLayout $"Results"