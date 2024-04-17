namespace StockScreenerReports.Web.Shared

module Views =
    open Giraffe
    open Giraffe.ViewEngine
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Core

    let billion = 1_000_000_000m
    let million = 1_000_000m

    type LinkTitle = string
    type LinkUrl = string

    type ColumnValue =
            | LinkColumn of LinkTitle * LinkUrl
            | LinkNewTabColumn of LinkTitle * LinkUrl
            | StringColumn of string
            | DateColumn of System.DateTime
            | NumberColumn of decimal
            | TickerLinkColumn of string
            | NodeColumn of XmlNode

    let marketCapFormatted marketCap =
        match marketCap with
        | d when d > billion -> System.Math.Round(d / billion, 2).ToString() + "B"
        | d -> System.Math.Round(d / million, 2).ToString() + "M"

    let marketCapOptionFormatted marketCapOption =
        match marketCapOption with
        | Some marketCap -> marketCapFormatted marketCap
        | None -> ""

    let dollarFormatted (value:decimal) =
        value.ToString("C", ReportsConfig.userCulture)

    let percentFormatted (value:decimal) =
        $"{value} %%"

    let volumeFormatted (value:int64) =
        value.ToString("N0")

    let generateIcon classNames =

        span [_class "icon"] [
            i [_class classNames] []
        ]

    let generateEarningsIcon hasEarnings =
        match hasEarnings with
        | true -> "fa-solid fa-e" |> generateIcon
        | false -> i [] []

    let generateTopGainerIcon isTopGainer =
        match isTopGainer with
        | true -> "fa-solid fa-fire has-text-success" |> generateIcon
        | false -> i [] []

    let generateNewHighIcon isNewHigh =
        match isNewHigh with
        | true -> "fa-solid fa-arrow-up has-text-success" |> generateIcon
        | false -> i [] []

    let generateNewLowIcon isNewLow =
        match isNewLow with
        | true -> "fa-solid fa-arrow-down has-text-danger" |> generateIcon
        | false -> i [] []

    let generateTopLoserIcon isTopLoser =
        match isTopLoser with
        | true -> "fa-solid fa-fire has-text-danger" |> generateIcon
        | false -> i [] []

    let generateTickerLink ticker =
        a [
            ticker |> Links.stockLink |> _href
        ] [
            str ticker
        ]

    let generateHref title link =
        a [
            _href link
        ] [
            str title
        ]

    let generateHrefWithElement element link =
        a [
            _href link
        ] [
            element
        ]

    let generateHrefWithAttr title link attr =
        a [
            _href link
            attr
        ] [
            str title
        ]

    let generateHrefNewTab title link =
        generateHrefWithAttr title link (_target "_blank")

    let toTdWithNodes nodes =
            td [] nodes

    let toTdWithNodeWithWidth (width:int) node =
        let style = $"width: {width}px"

        td [ _style style ] [node]

    let toTdWithNode node =
        [node] |> toTdWithNodes 

    let toHeaderCell title =
        th [] [str title]

    let toTd cell = 
        let node =
            match cell with
            | LinkColumn (title, link) -> generateHref title link
            | LinkNewTabColumn (title, link) -> generateHrefNewTab title link
            | StringColumn text -> str text
            | DateColumn date -> date.ToString("yyyy-MM-dd") |> str
            | NumberColumn number -> number.ToString("N2") |> str
            | TickerLinkColumn ticker -> generateTickerLink ticker
            | NodeColumn node -> node
        
        node |> toTdWithNode

    let toTr cells =
        tr [] (cells |> List.map toTd)

    let toSortableHeaderCell title = 
        th [ 
            _onclick $"sortBy(this)"
            _style "cursor: pointer"
        ] [str title]
    
    let generateHrefWithAttrs title link attributes =
        let finalAttributes = (_href link) :: attributes
        a finalAttributes [
            str title
        ]

    let private toNameCountRows breakdownName maxNumberOfRows nameElementFunc clickFunction list =
        let cellAttributes = match clickFunction with
                             | Some func -> [ _onclick func ]
                             | None -> []
        let rows =
            list
            |> List.truncate maxNumberOfRows
            |> List.map (fun (name,count) ->
                tr [] [
                    td cellAttributes [ (nameElementFunc name) ]
                    td [ _class "has-text-right"] [ str (count.ToString()) ]
                ])

        let header = tr [] [
            th [ _colspan "2"] [ str breakdownName ]
        ]

        (header, rows)

    let fullWidthTable headerCells rows =
        table [ _class "table is-fullwidth is-striped" ] [
            thead [] [ 
                tr [] (headerCells |> List.map toHeaderCell)
             ]
            tbody [] rows
        ]

    let fullWidthTableTextCentered headerCells rows =
        table [ _class "table is-fullwidth is-striped has-text-centered" ] [
            thead [] [ 
                tr [] (headerCells |> List.map toHeaderCell)
             ]
            tbody [] rows
        ]
    
    let fullWidthTableWithCustomHeader header rows =
        table [ _class "table is-fullwidth is-hoverable" ] [
            thead [] [ header ]
            tbody [] rows
        ]

    let fullWidthTableWithSortableHeaderCells headerCells rows =
        let headerRow =  tr [] (headerCells |> List.map toSortableHeaderCell)
        rows |> fullWidthTableWithCustomHeader headerRow

    let fullWidthTableWithCssClass cssClass header rows =
        table [ _class $"table is-fullwidth is-striped {cssClass}" ] [
            thead [] [ header ]
            tbody [] rows
        ]

    let toHeaderAndRowsWithLinksAndClickFunc title maxNumberOfRows linkFunction clickFunction listOfNameCountPairs =
        let header, rows = listOfNameCountPairs |> toNameCountRows title maxNumberOfRows (fun name -> generateHref name (linkFunction name)) clickFunction
        (header, rows)
    
    let toNameCountTableWithLinksAndClickFunc title maxNumberOfRows linkFunction clickFunction listOfNameCountPairs =
        let header, rows = toHeaderAndRowsWithLinksAndClickFunc title maxNumberOfRows linkFunction clickFunction listOfNameCountPairs
        rows |> fullWidthTableWithCustomHeader header

    type ClickFunction =
        | IndustryClicked | SectorClicked | CountryClicked
        
    module ClickFunction =        
        let javascriptFunction clickFunction =
            match clickFunction with
            | IndustryClicked -> Some "industryClicked(event)"
            | SectorClicked -> Some "sectorClicked(event)"
            | CountryClicked -> Some "countryClicked(event)"
    
    let toNameCountTableWithLinks title maxNumberOfRows linkFunction clickFunction listOfNameCountPairs =
        
        let onClickJavascript = ClickFunction.javascriptFunction clickFunction
            
        toNameCountTableWithLinksAndClickFunc title maxNumberOfRows linkFunction onClickJavascript listOfNameCountPairs

    let trendToHtml (trend:Trend) =
        let directionStr = 
            match trend.direction with
            | Up -> "Up"
            | Down -> "Down"

        $"Trending <b>{directionStr}</b> for <b>{trend.streak} days</b>, change of <b>{trend.change:N2}</b>, value of <b>{trend.value:N2}</b>"

    let marketCycleToHtml (cycle:MarketCycle) =
        $"Market cycle started on <b>{cycle.startPointDateFormatted}</b>, <b>{cycle.ageFormatted}</b> ago ({cycle.ageInMarketDays} market days), high of <b>{cycle.highPointValueFormatted} {cycle.highPointAgeFormatted} ago</b>"

    let private generateHeaderRow =
        nav [ _class "navbar header"; KeyValue("role", "navigation"); attr "aria-label" "main navigation" ] [
            div [ _class "navbar-brand" ] [
                a [ _class "navbar-item"; _href "/" ] [
                    encodedText "NGTD Screeners"
                ]
            ]

            div [ _id "navbarMenu"; _class "navbar-menu" ] [
                div [ _class "navbar-start" ] [
                ]
                
                div [_class "navbar-end"] [
                    div [_class "navbar-item"] [
                        form [ _action Links.searchLink; _method "GET"; _class "navbar-item" ] [
                            div [ _class "field has-addons" ] [
                                div [ _class "control has-icons-left is-expanded" ] [
                                    input [
                                        _class "input"
                                        _type "text"
                                        _placeholder "Search for stock or industry"
                                        _name "query"
                                    ]
                                    span [ _class "icon is-small is-left" ] [
                                        i [ _class "fas fa-search" ] []
                                    ]
                                ]
                                div [ _class "control" ] [
                                    button [ _class "button is-primary" ] [
                                        encodedText "Search"
                                    ]
                                ]
                            ]
                        ]
                    ]
                    
                    div [ _class "navbar-item has-dropdown is-hoverable" ] [
                        a [ _class "navbar-link" ] [
                            encodedText "Industries"
                        ]
                        
                        div [ _class "navbar-dropdown" ] [
                            generateHrefWithAttr "Industries" Links.industries (_class "navbar-item")
                            generateHrefWithAttr "Industries Table" Links.industriestable (_class "navbar-item")
                            generateHrefWithAttr "Industry Sequence Analysis" Links.industrySequenceAnalysis (_class "navbar-item")
                        ]
                    ]
                    
                    generateHrefWithAttr "Cycles" Links.cycles (_class "navbar-item")

                    div [ _class "navbar-item has-dropdown is-hoverable" ] [
                        a [ _class "navbar-link" ] [
                            encodedText "Market Data"
                        ]

                        div [ _class "navbar-dropdown" ] [
                            generateHrefWithAttr "Countries" Links.countries (_class "navbar-item")
                            generateHrefWithAttr "Earnings" Links.earnings (_class "navbar-item")
                        ]
                    ]
                ]
            ]
        ]

    let mainLayout pageTitle (content: XmlNode list) =
    
        let header = generateHeaderRow
        
        html [] [
            head [] [
                title []  [ encodedText pageTitle ]

                link [
                    _rel "stylesheet"
                    Links.bulmaCssLink |> _href
                ]

                link [
                    _rel "stylesheet"
                    "/styles.css" |> _href
                ]

                link [
                    _rel "stylesheet"
                    "/all.min.css" |> _href
                ]

                script [ Links.chartJsLink |> _src ] []
                script [ Links.chartJsDatalabelsLink |> _src ] []
                script [ Links.sortingJsLink |> _src ] []
                script [ Links.hidingJsLink |> _src ] []
                script ["https://cdn.plot.ly/plotly-latest.min.js" |> _src ] []

                meta [
                    _name "viewport"
                    _content "width=device-width, initial-scale=1"
                ]
            ]
            body [] [
                div [_class "container p-3"; _style "background-color: white"] (header::content)
            ]
        ] |> htmlView
    
    let notFound message =
        let view = 
            div [_class "content"] [
                h1 [] [
                    str message
                ]
            ]
        
        [view] |> mainLayout message

    let generateScreenerTags idAndNameTuple =
        let id,name = idAndNameTuple

        let backgroundColor = ReportsConfig.getBackgroundColorForScreenerId id

        div [ _class "tags has-addons" ] [
            span [ 
                _class $"tag "
                _style $"background-color: {backgroundColor};"
            ] [
                "" |> str
            ]
            span [ _class "tag" ] [
                name |> str
            ]
        ]

    let generateJobStatusDiv message =
        div [ _class "columns" ] [
            div [ _class "column" ] [ 
                message |> str 
            ]
        ]

    [<Literal>]
    let marketCycleScoreTm = "Cycle Score\u2122"
    [<Literal>]
    let trendScoreTm = "Trend Score\u2122"

    let getFilterSectionParams (ctx: Microsoft.AspNetCore.Http.HttpContext) =
        let startDateParam = ctx.TryGetQueryStringValue "startDate"
        let endDateParam = ctx.TryGetQueryStringValue "endDate"
        let dateAdjustmentParam = ctx.TryGetQueryStringValue "dateAdjustment"

        let dateRange = ReportsConfig.dateRangeAsStrings()

        let startDate = 
            match startDateParam with
                | Some s -> s
                | None -> dateRange |> fst

        let endDate =
            match endDateParam with
                | Some s -> s
                | None -> dateRange |> snd

        let dateAdjustmentParamValue =
            match dateAdjustmentParam with
                | Some s -> s
                | None -> ""

        let adjustment =
            match dateAdjustmentParamValue with
                | "" -> 0
                | _ -> int dateAdjustmentParamValue

        match adjustment with
            | 0 -> (startDate, endDate)  
            | _ ->
                let startDate = System.DateTime.Parse(startDate)
                let endDate = System.DateTime.Parse(endDate)
                let adjustedStart = startDate.AddDays(adjustment)
                let adjustedEnd = endDate.AddDays(adjustment)
                (adjustedStart |> Utils.convertToDateString, adjustedEnd |> Utils.convertToDateString)

    let generateFilterSection dateRange =
        let startDate, endDate = dateRange
        let offsets = [-30; -1; 1; 30]

        let buttons = 
            offsets
            |> List.indexed
            |> List.map( fun(index,offset) ->
                button [ _class "button is-small is-primary m-1"; _type "button"; _id $"applyFilters{index}" ] [ str $"{offset}" ]
            )

        let scripts = 
            offsets
            |> List.indexed
            |> List.map( fun(index,offset) ->
                script [ _type "text/javascript" ] [
                    rawText $"document.getElementById('applyFilters{index}').addEventListener('click', function() {{ document.getElementById('dateAdjustment').value = {offset}; document.getElementById('applyFilters').click(); }});"
                ]
            )

        let applyButton = button [ _class "button is-small is-primary m-1"; _type "submit"; _id "applyFilters" ] [ str "Apply" ]
        let resetButton = button [ _class "button is-small is-secondary m-1"; _onclick "window.navigation.navigate(document.location.pathname)" ] [ str "Reset" ]

        let formElements = [
            div [_class "field"] [
                label [_class "label is-small"; _for "startDate"] [str "Start Date"]
                input [ _class "input is-small"; _type "date"; _value startDate; _id "startDate"; _name "startDate" ]
            ]
            div [_class "field"] [
                label [_class "label is-small"; _for "endDate"] [str "End Date"]
                input [ _class "input is-small"; _type "date"; _value endDate; _id "endDate"; _name "endDate" ]
            ]
            input [ _class "input"; _type "hidden"; _value ""; _id "dateAdjustment"; _name "dateAdjustment"]
            div [_class "control"] ((applyButton::buttons) @ [resetButton])
        ]

        let toggleFunctions = "toggleDisplayNone(document.getElementById('filterSummary')); toggleDisplayNone(document.getElementById('filterForm'))"

        let form =
            form [
                _id "filterForm"
                _style "display:none;"
            ] ([formElements; scripts] |> List.concat)

        div [ _class "content"] [
            h4 [] [ str "Filters" ]
            div [
                _id "filterSummary"
                _onclick toggleFunctions
                ] [ $"<b>{startDate}</b> - <b>{endDate}</b>" |> rawText ]
            form
        ]

    let generateStockTable stocks =
        let stockTableHeaders = [
            "ticker"
            "company"
            "sector"
            "industry"
            "country"
            "market cap"
            "chart"
        ]

        stocks
        |> List.sortByDescending _.marketCap
        |> List.map (fun stock ->
            tr [] [
                TickerLinkColumn(stock.ticker |> StockTicker.value) |> toTd
                StringColumn(stock.company)   |> toTd
                LinkColumn(stock.sector, stock.sector |> Links.sectorLink) |> toTd
                LinkColumn(stock.industry, stock.industry |> Links.industryLink) |> toTd
                LinkColumn(stock.country, stock.country |> Links.countryLink) |> toTd
                StringColumn(stock.marketCap |> marketCapOptionFormatted) |> toTd
                LinkNewTabColumn("chart", stock.ticker |> StockTicker.value |> Links.tradingViewLink) |> toTd
            ]
        )
        |> fullWidthTableWithSortableHeaderCells stockTableHeaders

    let toSection title content =
        section [ _class "content" ] [
            h4 [] [ str title ]
            content
        ]

    let toSectionWithNoContent title =
        div [] [] |> toSection title

    let jobAlertSection missedJobs =
        
        let jobFailureContent job = 
            match job.status with
                | Success -> 
                    p [] [
                            str $"Job {job.name} has not run since {job.timestamp}"
                    ]
                | Warning -> 
                    p [ _class "has-text-warning" ] [
                        str $"Job {job.name} warning: {job.timestamp}"
                    ]
                | Failure -> 
                    p [] [
                        str $"Job {job.name} failed with message: {job.message}"
                    ]

        match missedJobs with
        | [] -> div [] []
        | _ ->
            div [ _class "container" ] [
                div [_class "notification is-danger mb-5"] 
                    (missedJobs |> List.map jobFailureContent)
            ]
            
    let sequenceToDurationBarChart (seq:IndustrySequence) =
        // background color should vary based on if the sequence is open or not
        let backgroundColor =
            match seq.open' with
            | true -> "#ff7f0e" // gold
            | false -> "#1f77b4" // blue
        
        [
            div [_style $"display: inline-block; background-color: {backgroundColor}; height: 20px; width: {seq.length * 20}px"] []
            span [_class "ml-3"] [str $"{seq.ageInDays} days"]
        ]
    
    let sentimentClass sentiment =
            match sentiment with
            | Positive -> "has-text-success"
            | Negative -> "has-text-danger"
            | Neutral -> "has-text-warning"
            
    let sentimentText sentiment =
        match sentiment with
        | Positive -> "📈"
        | Negative -> "📉"
        | Neutral -> "➖"