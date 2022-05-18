namespace FinvizScraper.Web.Handlers

module ScreenerManagement =

    open Giraffe.ViewEngine
    open FinvizScraper.Web.Shared
    open FinvizScraper.Storage
    
    let handler() = 
        
        let screeners = Storage.getScreeners()

        let screenerRows =
            screeners
            |> List.map ( fun screener ->
                tr [] [
                    td [] [str (screener.id.ToString())]
                    td [] [str screener.name]
                    td [] [screener.url |> Views.generateHref "Finviz" ]
                    td [] []
                ]
            )

        let rows = [
            thead [] [
                tr [] [
                    th [] [ str "Id" ]
                    th [] [ str "Name" ]
                    th [] [ str "Url" ]
                    th [] [ str "Actions" ]
                ]
            ]
            tbody [] screenerRows
        ]
        let screenerTable = rows |> Views.fullWidthTable

        let content =
            div [ _class "container" ] [
                div [ _class "content" ] [
                    h1 [ _class "title" ] [ str "Screener Management" ]
                ]
                screenerTable
            ]

        [content] |> Views.mainLayout "Screeners"