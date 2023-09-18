namespace StockScreenerReports.Web.Handlers

module ScreenerManagement =

    open FSharp.Data
    open Giraffe
    open Giraffe.ViewEngine
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Web.Shared.Views
    open StockScreenerReports.Core

    [<CLIMutable>]
    type ScreenerInput =
        {
            name: string
            url: string
        }
    
    [<CLIMutable>]
    type MigrateDateInput = 
        {
            fromdate: string
            todate: string
        }

    [<CLIMutable>]
    type DeleteDateInput = 
        {
            date: string
        }
        
    [<CLIMutable>]
    type RenameStockInput = 
        {
            oldTicker: string
            newTicker: string
        }

    type ScreenerExportType =   CsvProvider<
        Schema = "date, ticker, name, sector (string), industry (string), country (string), marketCap (decimal), price (decimal), change (decimal), volume (decimal), url (string)",
        HasHeaders=false>

    let header = "date, ticker, name, sector, industry, country, marketCap, price, change, volume, url"

    let deleteHandler id =
        let screener = Storage.getScreenerById id
        match screener with
        | Some s ->
            Storage.deleteScreener s |> ignore
            redirectTo false Links.screeners
        | None ->
            redirectTo false Links.screeners

    let exportHandler id =
        setHttpHeader "Content-Type" "text/csv"
        >=> 
            let screener = Storage.getScreenerById id
            let filename =
                match screener with
                | Some s -> $"export_{s.name}.csv"
                | None -> $"export.csv"

            let escapedFilename = System.Uri.EscapeDataString(filename)

            setHttpHeader "Content-Disposition" $"attachment; filename={escapedFilename}"
        >=>
            let data = Reports.getAllScreenerResults id
            let rows = 
                data 
                |> List.map (fun r -> 
                    ScreenerExportType.Row(
                        r.date,
                        r.ticker,
                        r.name,
                        r.sector,
                        r.industry,
                        r.country,
                        r.marketCap,
                        r.price,
                        r.change,
                        r.volume,
                        r.ticker |> Links.tradingViewLink
                    )
                )

            let csv = new ScreenerExportType(rows)

            setBodyFromString (header + System.Environment.NewLine + csv.SaveToString())

    let createHandler : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
            task {
                let! input = ctx.BindFormAsync<ScreenerInput>()
                Storage.saveScreener input.name input.url |> ignore
                return! redirectTo false Links.screeners next ctx
            }

    let migrateDateHandler : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
            task {
                let! input = ctx.BindFormAsync<MigrateDateInput>()
                let fromdate = input.fromdate
                let todate = input.todate
                
                Storage.migrateDates fromdate todate |> ignore
                
                return! redirectTo false Links.screeners next ctx
            }

    let deleteDateHandler : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
            task {
                let! input = ctx.BindFormAsync<DeleteDateInput>()
                input.date |> Storage.deleteDate |> ignore
                return! redirectTo false Links.screeners next ctx
            }
            
    let renameStockHandler : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
            task {
                let! input = ctx.BindFormAsync<RenameStockInput>()
                Storage.renameStockTicker (input.oldTicker |> StockTicker.create) (input.newTicker |> StockTicker.create) |> ignore
                return! redirectTo false Links.screeners next ctx
            }

    let createScreenersTable (screeners:list<StockScreenerReports.Core.Screener>) =
        let screenerRows =
            screeners
            |> List.map ( fun screener ->
                tr [] [
                    StringColumn(screener.id.ToString()) |> toTd
                    td [] [
                        div [] [str screener.name]
                        div [] [screener.url |> generateHrefNewTab screener.url]
                    ]
                    td [] [
                        
                        generateHrefWithAttr
                            "Results"
                            (screener.id |> Links.screenerLink)
                            (_class "button is-primary is-small is-light is-pulled-left mr-2")

                        form [
                            _method "POST"
                            _action (Links.screenersExport screener.id)
                        ] [
                            input [
                                _type "submit"
                                _value "Export"
                                _class "button is-primary is-small is-light is-pulled-left"
                            ]
                        ]

                        form [
                            _method "POST"
                            _action (Links.screenersDelete screener.id)
                        ] [
                            input [
                                _type "submit"
                                _value "Delete"
                                _class "button is-danger is-small is-light is-pulled-right"
                                _onclick "return confirm('Are you sure you want to delete this screener?')"
                            ]
                        ]
                    ]
                ]
            )

        let tableHeader = 
            tr [] [
                "Id" |> toHeaderCell
                th [ _width "400" ] [ str "Name" ]
                "" |> toHeaderCell
            ]
                
        screenerRows
        |> fullWidthTableWithCustomHeader tableHeader

    let createNewScreenerForm =
        form [
                _method "POST"
                _action Links.screenersNew
            ] [
                div [ _class "field" ] [
                    label [ _for "name" ] [ str "Name" ]
                    input [
                        _type "text"
                        _name "name"
                        _class "input"
                    ]
                ]
                div [ _class "field" ] [
                    label [ _for "url" ] [ str "Url" ]
                    input [
                        _type "text"
                        _name "url"
                        _class "input"
                    ]
                ]
                // submit button
                input [
                    _type "submit"
                    _class "button is-primary"
                    _value "Create"
                ]
            ]
        
    let createRenameStockForm =
        form [
                _method "POST"
                _action Links.renameStockLink
            ] [
                div [ _class "field" ] [
                    label [ _for "oldTicker" ] [ str "Old Ticker" ]
                    input [
                        _type "text"
                        _name "oldTicker"
                        _class "input"
                    ]
                ]
                div [ _class "field" ] [
                    label [ _for "newTicker" ] [ str "New Ticker" ]
                    input [
                        _type "text"
                        _name "newTicker"
                        _class "input"
                    ]
                ]
                // submit button
                input [
                    _type "submit"
                    _class "button is-primary"
                    _value "Rename"
                ]
            ]

    let createDeleteForm =
        form [
                _method "POST"
                _action Links.deleteDateLink
            ] [
                div [ _class "field" ] [
                    label [ _for "date" ] [ str "Date" ]
                    input [
                        _type "date"
                        _name "date"
                        _class "input"
                    ]
                ]
                // submit button
                input [
                    _type "submit"
                    _class "button is-danger"
                    _value "Delete Date"
                ]
            ]
    let createMigrateForm =
        form [
                _method "POST"
                _action Links.migrateDateLink
            ] [
                div [ _class "field" ] [
                    label [ _for "fromdate" ] [ str "From Date" ]
                    input [
                        _type "date"
                        _name "fromdate"
                        _class "input"
                    ]
                ]
                div [ _class "field" ] [
                    label [ _for "todate" ] [ str "To Date" ]
                    input [
                        _type "date"
                        _name "todate"
                        _class "input"
                    ]
                ]
                // submit button
                input [
                    _type "submit"
                    _class "button is-primary"
                    _value "Migrate"
                ]
            ]

    let createJobsTable (jobs:list<StockScreenerReports.Core.Job>) =
        let getLink (job:StockScreenerReports.Core.Job) =
            match job.name with
            | EarningsJob -> Links.jobsEarnings
            | ScreenerJob -> Links.jobsScreeners
            | TrendsJob -> Links.jobsTrends
            | _ -> job.name.ToString()

        let jobRows =
            jobs
            |> List.map ( fun job ->
                let classToSet = 
                    match job.status with
                    | Success -> ""
                    | Failure -> "has-text-danger-dark"

                tr [_class classToSet] [
                    StringColumn(job.name.ToString()) |> toTd
                    StringColumn(job.message) |> toTd
                    StringColumn(job.status.ToString()) |> toTd
                    StringColumn(job.timestamp.ToString()) |> toTd
                    LinkColumn("Run", job |> getLink) |> toTd
                ]
            )

        let header = ["Name"; "Message"; "Status"; "Timestamp"; ""]
                
        jobRows
        |> fullWidthTable header

    let managementHandler() = 
        
        let screeners = Storage.getScreeners()
        let jobs = Storage.getJobs()

        let screenerTable = createScreenersTable screeners

        let newScreenerForm = createNewScreenerForm

        let migrateForm = createMigrateForm

        let deleteDateForm = createDeleteForm
        
        let renameStockForm = createRenameStockForm

        let jobsTable = createJobsTable jobs

        let content =
            div [ _class "container" ] [
                screenerTable   |> toSection "Screener Management"
                newScreenerForm |> toSection "New Screener"
                migrateForm     |> toSection "Migrate"
                deleteDateForm  |> toSection "Delete Date"
                renameStockForm |> toSection "Rename Stock"
                jobsTable       |> toSection "Jobs"
            ]

        [content] |> mainLayout "Screeners"