namespace FinvizScraper

module Config =

    let readConfig filepath =
        System.Text.Json.JsonSerializer.Deserialize<FinvizConfig>(
            System.IO.File.ReadAllText(filepath)
        )