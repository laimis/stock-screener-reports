namespace FinvizScraper.Web.Handlers

module ScreenerManagement =

    open Giraffe
    open Giraffe.ViewEngine
    open FinvizScraper.Web.Shared
    open FinvizScraper.Storage
    open FSharp.Data

    [<CLIMutable>]
    type ScreenerInput =
        {
            name: string
            url: string
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

    let managementHandler() = 
        
        let screeners = Storage.getScreeners()

        let screenerRows =
            screeners
            |> List.map ( fun screener ->
                tr [] [
                    td [] [str (screener.id.ToString())]
                    td [] [
                        div [] [str screener.name]
                        div [] [screener.url |> Views.generateHrefNewTab screener.url]
                    ]
                    td [] [
                        
                        Views.generateHrefWithAttr
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

        let rows = [
            thead [] [
                tr [] [
                    th [] [ str "Id" ]
                    th [ _width "400" ] [ str "Name" ]
                    th [] []
                ]
            ]
            tbody [] screenerRows
        ]
        let screenerTable = rows |> Views.fullWidthTable

        let newScreenerForm =
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

        let content =
            div [ _class "container" ] [
                div [ _class "content" ] [
                    h1 [] [ str "Screener Management" ]
                ]
                screenerTable
                div [ _class "content" ] [
                    h2 [] [ str "Add New" ]
                ]
                newScreenerForm
            ]

        [content] |> Views.mainLayout "Screeners"