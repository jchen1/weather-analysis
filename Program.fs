open System
open FSharp.Data
open Plotly.NET
open Plotly.NET.LayoutObjects

type Row =
    { Date: DateOnly
      StationName: string
      Precipitation: decimal option
      SnowDepth: decimal option
      Snowfall: decimal option
      AverageTemperature: decimal option
      MaximumTemperature: decimal option
      MinimumTemperature: decimal option
      WaterEquivalentOfSnowOnGround: decimal option
      WaterEquivalentOfSnowfall: decimal option
      AverageWindSpeed: decimal option
      FastestTwoMinuteWindDirection: string option
      FastestFiveMinuteWindDirection: string option
      FastestTwoMinuteWindSpeed: decimal option
      FastestFiveMinuteWindSpeed: decimal option
      WeatherCodes: (string list) option }

let file = CsvFile.Load("data.csv", hasHeaders = true).Cache()

let stationOrder =
    [ "LIVINGSTON RAIN GAUGE, MT US"
      "LIVINGSTON AIRPORT, MT US"
      "LIVINGSTON 2.3 SSW, MT US"
      "LIVINGSTON 0.4 SSW, MT US"
      "LIVINGSTON 0.9 WSW, MT US"
      "LIVINGSTON 12 S, MT US" ]
    |> Seq.mapi (fun i x -> (x, i))
    |> dict

let bestValueForDate (rows: 'U seq) (f: 'U -> ('T option)) =
    rows |> Seq.map f |> Seq.filter Option.isSome |> Seq.tryHead |> Option.flatten

let rows =
    file.Rows
    |> Seq.map (fun row ->
        { Date = DateOnly.Parse(row.GetColumn "DATE")
          StationName = row.GetColumn "NAME"
          Precipitation =
            try
                Some(decimal (row.GetColumn "PRCP"))
            with _ ->
                None
          SnowDepth =
            try
                Some(decimal (row.GetColumn "SNWD"))
            with _ ->
                None
          Snowfall =
            try
                Some(decimal (row.GetColumn "SNOW"))
            with _ ->
                None
          AverageTemperature =
            try
                Some(decimal (row.GetColumn "TAVG"))
            with _ ->
                None
          MaximumTemperature =
            try
                Some(decimal (row.GetColumn "TMAX"))
            with _ ->
                None
          MinimumTemperature =
            try
                Some(decimal (row.GetColumn "TMIN"))
            with _ ->
                None
          WaterEquivalentOfSnowOnGround =
            try
                Some(decimal (row.GetColumn "WESD"))
            with _ ->
                None
          WaterEquivalentOfSnowfall =
            try
                Some(decimal (row.GetColumn "WESF"))
            with _ ->
                None
          AverageWindSpeed =
            try
                Some(decimal (row.GetColumn "AWND"))
            with _ ->
                None
          FastestTwoMinuteWindDirection = None
          FastestFiveMinuteWindDirection = None
          FastestTwoMinuteWindSpeed =
            try
                Some(decimal (row.GetColumn "WSF2"))
            with _ ->
                None
          FastestFiveMinuteWindSpeed =
            try
                Some(decimal (row.GetColumn "WSF5"))
            with _ ->
                None
          WeatherCodes = None })
    |> Seq.groupBy (fun row -> row.Date)
    |> Seq.map (fun (date, rows) ->
        let rows = rows |> Seq.sortBy (fun r -> stationOrder.Item(r.StationName))

        { Date = date
          StationName = "Combined"
          Precipitation = bestValueForDate rows (fun r -> r.Precipitation)
          SnowDepth = bestValueForDate rows (fun r -> r.SnowDepth)
          Snowfall = bestValueForDate rows (fun r -> r.Snowfall)
          AverageTemperature = bestValueForDate rows (fun r -> r.AverageTemperature)
          MaximumTemperature = bestValueForDate rows (fun r -> r.MaximumTemperature)
          MinimumTemperature = bestValueForDate rows (fun r -> r.MinimumTemperature)
          WaterEquivalentOfSnowOnGround = bestValueForDate rows (fun r -> r.WaterEquivalentOfSnowOnGround)
          WaterEquivalentOfSnowfall = bestValueForDate rows (fun r -> r.WaterEquivalentOfSnowfall)
          AverageWindSpeed = bestValueForDate rows (fun r -> r.AverageWindSpeed)
          FastestTwoMinuteWindDirection = bestValueForDate rows (fun r -> r.FastestTwoMinuteWindDirection)
          FastestFiveMinuteWindDirection = bestValueForDate rows (fun r -> r.FastestFiveMinuteWindDirection)
          FastestTwoMinuteWindSpeed = bestValueForDate rows (fun r -> r.FastestTwoMinuteWindSpeed)
          FastestFiveMinuteWindSpeed = bestValueForDate rows (fun r -> r.FastestFiveMinuteWindSpeed)
          WeatherCodes = bestValueForDate rows (fun r -> r.WeatherCodes) })


let firstSnow =
    rows
    |> Seq.groupBy (fun row -> row.Date.Year)
    |> Seq.map (fun (year, rows) ->
        // assume "first snow" is always after august
        let dateOnly =
            (rows
             |> Seq.filter (fun row -> row.Date.Month > 7 && (Option.defaultValue 0m row.Snowfall) > 0m)
             |> Seq.sortBy (fun row -> row.Date.DayNumber)
             |> Seq.head)
                .Date
        // ignore the year so we graph in the same window
        (year, DateOnly(2000, dateOnly.Month, dateOnly.Day).ToDateTime(TimeOnly.MinValue)))

let lastSnow =
    rows
    |> Seq.groupBy (fun row -> row.Date.Year)
    |> Seq.map (fun (year, rows) ->
        rows
        |> Seq.filter (fun row -> row.Date.Month <= 7 && (Option.defaultValue 0m row.Snowfall) > 0m)
        |> Seq.sortBy (fun row -> -1 * row.Date.DayNumber)
        |> Seq.tryHead)
    |> Seq.filter Option.isSome
    |> Seq.map (fun dt ->
        DateOnly(2000, dt.Value.Date.Month, dt.Value.Date.Day)
            .ToDateTime(TimeOnly.MinValue))

let charts =
    [ firstSnow
      |> Chart.Point
      |> Chart.withXAxisStyle "First snowfall of the year (by year)"
      firstSnow
      |> Seq.map snd
      |> (fun d -> Chart.Histogram(d, NBinsX = 100, NBinsY = 100))
      |> Chart.withXAxis (LinearAxis.init (TickFormat = "%b-%d"))
      |> Chart.withXAxisStyle "First snowfall of the year (histogram)"
      lastSnow
      |> (fun d -> Chart.Histogram(d, NBinsX = 100, NBinsY = 100))
      |> Chart.withXAxis (LinearAxis.init (TickFormat = "%b-%d"))
      |> Chart.withXAxisStyle "Last snowfall of the year (histogram)" ]

charts
|> Chart.SingleStack(Pattern = StyleParam.LayoutGridPattern.Independent, YGap = 15)
|> Chart.withSize (Width = 1024)
|> Chart.withLegend false
|> Chart.show

// block so the chart shows up...
Console.ReadLine() |> ignore
