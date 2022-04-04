namespace FinvizScraper.Web.Handlers

module Dashboard =

    open Giraffe.ViewEngine
    open FinvizScraper.Storage.Reports

    type DashboardViewModel =
        {
            screener:ScreenerResultReport;
            sectors:list<(string * int)>;
            industries:list<(string * int)>;
            countries:list<(string * int)>;
        }

    let private generateBreakdownParts screener = 
        let toNameCountRows breakdownName list =
            let rows =
                list
                |> List.map (fun (name,count) ->
                    tr [] [
                        td [] [ str name ]
                        td [] [ str (count.ToString()) ]
                    ])

            let header = tr [] [
                th [ _colspan "2"] [ str breakdownName ]
            ]

            header::rows

        let toTable breakdownName list =
            table [Shared.fullWidthTableAttributes] (list |> toNameCountRows breakdownName)

        let sectorsTable = screener.sectors |> toTable "Sectors"
        let industriesTable = screener.industries |> toTable "Industries"
        let countriesTable = screener.countries |> toTable "Countries"

        let screenerDate = screener.screener.date.ToString("yyyy-MM-dd")
        
        div [_class "content"] [
            h2 [] [ str screener.screener.name ]
            h5 [] [
                div [ _class "buttons"] [
                    a [ _class "button is-primary"
                        _href $"/screeners/{screener.screener.screenerid}/results/{screenerDate}"] [
                        str (screener.screener.count.ToString() + " results")
                    ]
                    a [ _class "button is-primary"
                        _href $"/screeners/{screener.screener.screenerid}"] [
                        str "Screener Details"
                    ]
                ]
            ]
            div [_class "columns"] [
                div [_class "column"] [sectorsTable]
                div [_class "column"] [industriesTable]
                div [_class "column"] [countriesTable]
            ]
        ]

    let private view (screeners:list<DashboardViewModel>) =
        let screenerRows =
            screeners
            |> List.map generateBreakdownParts

        let nodes = [
            h1 [_class "title"] [ str "Dashboard" ]
        ]

        let allNodes = List.append nodes screenerRows

        allNodes |> Shared.mainLayout "Dashboard"

    let handler()  = 
        
        // get screeners, render them in HTML
        let screenerResults = getLatestScreeners()

        let screenerResultWithBreakdowns =
            screenerResults
            |> List.map (fun x -> 
                let sectorBreakdown = topSectors x.screenerid x.date
                let industryBreakdown = topIndustries x.screenerid x.date
                let countryBreakdown = topCountries x.screenerid x.date
                {
                    screener=x;
                    sectors=sectorBreakdown;
                    industries=industryBreakdown;
                    countries=countryBreakdown
                }
            )

        let view      = view screenerResultWithBreakdowns
        Giraffe.Core.htmlView view