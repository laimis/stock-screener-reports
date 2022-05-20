namespace FinvizScraper.Web.Shared

module Links =

    // external links
    let tradingViewLink ticker =
        $"https://tradingview.com/chart/kQn4rgoA/?symbol={ticker}"

    let bulmaCssLink = "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css"

    let chartJsLink = "https://cdn.jsdelivr.net/npm/chart.js"

    let chartJsDatalabelsLink = "https://cdn.jsdelivr.net/npm/chartjs-plugin-datalabels"


    let home = "/"

    let finvizLogoLink = "/finviz_logo.png"

    // app links
    let stockLink ticker =
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
    
    let screenerTrends = "/screeners/trends"
    let industryTrends = "/industries/trends"

    let industryFinvizLink (industryName:string) =
        let value = industryName.Replace("&", "").Replace(" ", "").ToLower()
        $"https://finviz.com/screener.ashx?v=111&f=ind_" + value