namespace StockScreenerReports.Web.Shared

module Links =

    // external links
    let tradingViewLink ticker =
        $"https://tradingview.com/chart/kQn4rgoA/?symbol={ticker}"

    let bulmaCssLink = "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css"

    let chartJsLink = "https://cdn.jsdelivr.net/npm/chart.js"

    let sortingJsLink = "/scripts/sorting.js"
    let hidingJsLink = "/scripts/hiding.js"

    let chartJsDatalabelsLink = "https://cdn.jsdelivr.net/npm/chartjs-plugin-datalabels"


    let home = "/"

    let finvizLogoLink = "/finviz_logo.png"

    // app links
    let stockLink (ticker:string) =
        $"/stocks/{ticker}"

    let screenerLink screenerId =
        $"/screeners/{screenerId}"

    let screenerResultsLink screenerId date =
         $"/screeners/{screenerId}/results/{date}"

    let sectorLink sectorName =
        $"/sectors/{sectorName}"

    let industryLink industryName =
        $"/industries/{industryName}"

    let countryLink countryName =
        $"/countries/{countryName}"
    
    let screeners = "/screeners"
    let screenersNew = "/screeners/new"
    let screenersDelete id = $"/screeners/{id}/delete"
    let screenersExport id = $"/screeners/{id}/export"
    let adhocReportExport = "/reports/adhoc/export"
    
    let screenerTrends = "/screeners/trends"
    let industries = "/industries"
    let countries = "/countries"
    let earnings = "/earnings"

    let industryFinvizLink (industryName:string) =
        let value =
            industryName
            |> StockScreenerReports.Core.Utils.cleanIndustry
        
        $"https://finviz.com/screener.ashx?v=111&f=ind_" + value

    let ngtdOutcomesReportLink (title,tickers,earnings,date) =
        let commaSeparated = tickers |> String.concat ","
        let earningsCommaSeparated = earnings |> String.concat ","

        // https://localhost:5002
        $"https://ngtrading-xiu9e.ondigitalocean.app/reports/outcomes?tickers={commaSeparated}&earnings={earningsCommaSeparated}&title={title}&endDate={date}"