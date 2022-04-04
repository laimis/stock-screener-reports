namespace FinvizScraper.Web.Handlers

module ScreenerDashboard =

    open Giraffe.ViewEngine
    open FinvizScraper.Web.Handlers.Shared

    
    let handler screenerId  = 
        
        let byIdOption = FinvizScraper.Storage.Storage.getScreenerById screenerId
        match byIdOption with
        | Some screener -> 
            let view = div [_class "content"] [
                    h1 [] [
                        str screener.name
                    ]
                ]
            Giraffe.Core.htmlView ([view] |> mainLayout $"Screener: {screener.name}")
        | None ->
            Giraffe.Core.htmlView (notFound "Screener not found")