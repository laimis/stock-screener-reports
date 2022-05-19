namespace FinvizScraper.Web.Handlers

module ScreenerManagement =

    open Giraffe
    open Giraffe.ViewEngine
    open FinvizScraper.Web.Shared
    open FinvizScraper.Storage

    [<CLIMutable>]
    type ScreenerInput =
        {
            name: string
            url: string
        }

    let deleteHandler id =
        let screener = Storage.getScreenerById id
        match screener with
        | Some s ->
            Storage.deleteScreener s |> ignore
            redirectTo false Links.screeners
        | None ->
            redirectTo false Links.screeners

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
                    td [] [str screener.name]
                    td [] [screener.url |> Views.generateHref "Finviz" ]
                    td [] [
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
                    th [] [ str "Name" ]
                    th [] [ str "Url" ]
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