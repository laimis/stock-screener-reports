namespace FinvizScraper.Web.Handlers

module CountryDashboard =
    open FinvizScraper.Web.Shared
    open Giraffe.ViewEngine.HtmlElements
    open FinvizScraper.Storage
    open Giraffe.ViewEngine.Attributes
    open FinvizScraper.Core

    let handler countryName =
        let screeners = Storage.getScreeners()

        let days = FinvizConfig.dayRange

        let list = days |> Logic.businessDatesWithZeroPairs

        let charts = 
            screeners
            |> List.map (fun screener ->
                let data = Reports.getDailyCountsForScreenerAndCountry screener.id countryName days

                let mapped = data |> Map.ofList

                list
                |> List.map(fun (date,count) ->
                    let found = mapped.TryFind date
                    match found with
                    | Some c -> (date,c)
                    | None -> (date,count)
                )
                |> Charts.convertNameCountsToChart screener.name Charts.Bar None Charts.smallChart (FinvizConfig.getBackgroundColorForScreenerId screener.id)
                |> div [_class "block"] 
            )

        let view = 
            div [_class "content"] [
                h1 [] [
                    str countryName
                ]
            ]::charts
        
        view |> Views.mainLayout $"Country Dashboard for {countryName}" 