#r "../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
open FSharp.Data

[<Literal>]
let BeerPath = @"../data/untappd-goatie.csv"

type BeerCSV =
    CsvProvider<
        Sample = BeerPath,
        Separators = ",",
        Schema = "string,string,string,float,float,float,string">

type Beer = BeerCSV.Row

let initialBeers = BeerCSV.GetSample().Rows

let Beers = initialBeers |> Seq.filter(fun beer -> beer.IBU <= 100.0 && beer.IBU > 0.0)

let averageRating =
    Beers
    |> Seq.map (fun beer -> beer.``Rating``)
    |> Seq.average

let sampleSize =
    Beers |> Seq.length

#I "../packages/Newtonsoft.Json/lib/net45/"
#I "../packages/Google.DataTable.Net.Wrapper/lib/"
#r "../packages/XPlot.GoogleCharts/lib/net45/XPlot.GoogleCharts.dll"

open XPlot.GoogleCharts

let options = Configuration.Options()
options.dataOpacity <- 0.50
options.pointSize <- 8

Beers
|> Seq.map (fun beer -> beer.ABV, beer.``Rating``)
|> Chart.Scatter
|> Chart.WithOptions options
|> Chart.WithXTitle "ABV"
|> Chart.WithYTitle "Rating"
|> Chart.Show

Beers
|> Seq.map (fun beer -> beer.IBU, beer.Rating)
|> Chart.Scatter
|> Chart.WithOptions options
|> Chart.WithXTitle "IBU"
|> Chart.WithYTitle "Rating"
|> Chart.Show

Beers
|> Seq.map (fun beer -> beer.IBU, beer.ABV)
|> Chart.Scatter
|> Chart.WithOptions options
|> Chart.WithXTitle "IBU"
|> Chart.WithYTitle "ABV"
|> Chart.Show

let learnIBUStump ibu =
    // average quality for beers with IBU <= ibu
    let valueIfLow =
        Beers

        |> Seq.filter (fun beer -> beer.IBU < ibu)
        |> Seq.averageBy (fun beer -> beer.Rating)
    // average quality for beers with IBU > ibu
    let valueIfHigh =
        Beers

        |> Seq.filter( fun beer -> beer.IBU >= ibu)
        |> Seq.averageBy (fun beer -> beer.Rating)
    // create a stump
    let predictor (beer:Beer) =
        if beer.IBU < ibu
        then valueIfLow
        else valueIfHigh
    // return the stump
    predictor

let stump1 = learnIBUStump 10.0
let stump2 = learnIBUStump 90.0

// See what's happening on a plot
let actuals =
    Beers
    |> Seq.map (fun beer -> beer.IBU,beer.Rating)
let predictions1 =
    Beers
    |> Seq.map (fun beer -> beer.IBU, stump1 beer)
let predictions2 =
    Beers
    |> Seq.map (fun beer -> beer.IBU, stump2 beer)

[ actuals; predictions1; predictions2 ]
|> Chart.Scatter
|> Chart.WithOptions options
|> Chart.Show

let cost1 =
    Beers
    |> Seq.averageBy (fun beer ->
        pown ((beer.Rating)-(stump1 beer)) 2)

let cost2 =
    Beers
    |> Seq.averageBy (fun beer ->
        pown ((beer.Rating)-(stump2 beer)) 2)

Beers
|> Seq.take 50
|> Seq.iter (fun beer ->
    let rating = beer.Rating
    let predicted = stump1 beer
    let cost = pown (rating - predicted) 2
    printfn "Rating: %.1f Pred: %.1f Cost: %.1f" rating predicted cost
    )

let ibus =
    let values =
        Beers

        |> Seq.map (fun beer -> beer.IBU)
    let min = values |> Seq.min
    let max = values |> Seq.max
    let width = max - min
    let step = width / 20.0
    [ min + step .. step .. max - step ]

let bestStump =
    ibus
    |> Seq.map (learnIBUStump)
    |> Seq.minBy (fun stump ->
        Beers
        |> Seq.averageBy (fun beer ->
            pown ((beer.Rating)-(stump beer)) 2))

let bestCost =
    Beers
    |> Seq.averageBy (fun beer ->
        pown ((beer.Rating)-(bestStump beer)) 2)



Beers
|> Seq.map (fun beer -> beer.Rating, bestStump beer)
|> Chart.Scatter
|> Chart.WithOptions options
|> Chart.WithTitle "Actual vs. Predicted"
|> Chart.WithXTitle "Rating"
|> Chart.WithYTitle "Predicted"
|> Chart.Show

Beers
|> Seq.map (fun beer ->
    beer.IBU, beer.Rating - bestStump beer)
|> Chart.Scatter
|> Chart.WithOptions options
|> Chart.WithTitle "IBUs vs. Error"
|> Chart.WithXTitle "IBUs"
|> Chart.WithYTitle "Error"
|> Chart.Show
