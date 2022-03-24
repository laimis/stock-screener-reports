namespace FinvizScraper

module Config =

    let readConfig filepath =
        System.Text.Json.JsonSerializer.Deserialize<FinvizConfig>(
            System.IO.File.ReadAllText(filepath)
        )

    let breakdowns = [
        {
            name = "Sectors";
            breakdown = fun a -> a.sector
        }
        {
            name = "Industries";
            breakdown = fun a -> a.industry
        }
        {
            name = "Countries";
            breakdown = fun a -> a.country
        }
    ]