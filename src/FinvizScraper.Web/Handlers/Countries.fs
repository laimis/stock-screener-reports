namespace FinvizScraper.Web.Handlers

module Countries =
    open FinvizScraper.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open Giraffe.ViewEngine.Attributes
    open FinvizScraper.Storage

    let handler()  =
        
        let header = 
            div [_class "content"] [
                // h1 [] [
                //     str "Countries Dashboard"
                // ]
            ]

        let countries = Reports.getStockByCountryBreakdown()

        let rows =
            countries
            |> List.map (fun (country,count) ->
                tr [] [
                    country |> Links.countryLink |>  Views.generateHref country |> Views.toTdWithNode
                    Views.toTd (count.ToString())
                ]
            )

        let table = rows |> Views.fullWidthTable

        
        [header; table] |> Views.mainLayout $"Countries" 