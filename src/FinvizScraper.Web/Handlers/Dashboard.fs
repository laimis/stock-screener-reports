namespace FinvizScraper.Web.Handlers

module Dashboard =

    open Giraffe.ViewEngine
    open FinvizScraper.Storage.Reports
    open FinvizScraper.Web.Shared
    type DashboardViewModel =
        {
            screener:ScreenerResultReport;
            sectors:list<(string * int)>;
            industries:list<(string * int)>;
            countries:list<(string * int)>;
        }

    let private generateBreakdownParts screener = 
        
        let sectorsTable = screener.sectors |> Views.toNameCountTableWithLinks "Sectors" (fun name -> Links.sectorLink name)
        let industriesTable = screener.industries |> Views.toNameCountTableWithLinks "Industries" (fun name -> Links.industryLink name)
        let countriesTable = screener.countries |> Views.toNameCountTableWithLinks "Countries" (fun name -> Links.countryLink name)

        let screenerDate = screener.screener.date.ToString("yyyy-MM-dd")
        
        div [_class "content"] [
            h2 [] [ str screener.screener.name ]
            h5 [] [
                div [ _class "buttons"] [
                    
                    Views.generateHrefWithAttr
                        $"{screener.screener.count} results"
                        (Links.screenerResultsLink (screener.screener.screenerid) screenerDate)
                        (_class "button is-primary")

                    Views.generateHrefWithAttr
                        "Screener Details"
                        (screener.screener.screenerid |> Links.screenerLink)
                        (_class "button is-primary")
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
            div [ _class "columns" ] [
                div [ _class "column" ] [
                    h1 [_class "title"] [ str "Dashboard" ]
                ]
                div [ _class "column is-three-quarters" ] [
                    div [ _class "columns"] [
                        div [ _class "column" ] [
                            form [
                                _action "/stocks/search"
                                _method "GET"
                            ] [
                                input [
                                    _class "input"
                                    _type "text"
                                    _placeholder "Search for stock"
                                    _name "ticker"
                                ]
                            ]
                        ]
                        div [ _class "column" ] [
                            Views.generateHrefWithAttr
                                "Screener Trends"
                                Links.screenerTrends
                                (_class "button is-primary is-pulled-right mx-1")

                            Views.generateHrefWithAttr
                                "Industry Trends"
                                Links.industryTrends
                                (_class "button is-primary is-pulled-right mx-1")
                        ]
                    ]
                ]
            ]
            
        ]

        List.append nodes screenerRows

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

        view screenerResultWithBreakdowns
        |> Views.mainLayout "Dashboard"