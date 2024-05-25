namespace Project18

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating
open WebSharper.Sitelets



type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument>

   type EndPoint =
    | [<EndPoint "/">] Home
    | [<EndPoint "/counter">] Counter

[<JavaScript>]
module Pages =
    open System


   // Variables related to income
    let incomegroup = Var.Create ""        // Represents an income group as a string
    let gain = Var.Create 0.0              // Represents gain as a float
    let totalG = Var.Create 0.0            // Represents total gain as a float

    // Variables related to expenses
    let expensegroup = Var.Create ""       // Represents an expense group as a string
    let loss = Var.Create 0.0              // Represents loss as a float
    let totalE = Var.Create 0.0            // Represents total expenses as a float

    // Storage for income and expense data, each as a tuple of two lists
    let storage_i = Var.Create ([], [])    // Income-related data storage
    let storage_e = Var.Create ([], [])    // Expense-related data storage


     /// Adds a transaction to the given dataset and updates the total value.
    /// - `totalValue`: The current total value.
    /// - `categoryValue`: The category of the new transaction.
    /// - `amountValue`: The amount of the new transaction.
    /// - `(labels, data)`: A tuple containing the current labels and data.
    let addTransaction totalValue categoryValue amountValue (labels, data) =
        let newTotalValue = totalValue + amountValue
        let newLabels = labels @ [categoryValue]
        let newData = data @ [amountValue]
        (newLabels, newData, newTotalValue)

    /// Adds a new income transaction and updates the storage and total gain.
    let addIncome () =
        let (newLabels, newData, newTotalIncome) = 
            addTransaction totalG.Value incomegroup.Value gain.Value storage_i.Value

        storage_i.Value <- (newLabels, newData)
        totalG.Value <- newTotalIncome

        // Reset the input fields for the next transaction
        incomegroup.Value <- ""
        gain.Value <- 0.0

    /// Adds a new expense transaction and updates the storage and total expense.
    let addExpenses () =
        let (newLabels, newData, newTotalExpenses) = 
            addTransaction totalE.Value expensegroup.Value loss.Value storage_e.Value

        storage_e.Value <- (newLabels, newData)
        totalE.Value <- newTotalExpenses

        // Reset the input fields for the next transaction
        expensegroup.Value <- ""
        loss.Value <- 0.0

    // Computes the balance view by subtracting total expenses from total income
    let balanceView = 
        View.Map2 
            (fun income expense -> sprintf "%.2f" (income - expense)) 
            totalG.View 
            totalE.View

    // Formats the total income for display
    let totalIncomeView = 
        View.Map 
            (sprintf "%.2f") 
            totalG.View

    // Formats the total expenses for display
    let totalExpensesView = 
        View.Map 
            (sprintf "%.2f") 
            totalE.View

    let Homepage() =
        IndexTemplate.MoneyTracking()
            .incomegroup(incomegroup)
            .gains(gain)
            .expensegroup(expensegroup)
            .losses(loss)
            .Income(totalIncomeView)
            .Expenses(totalExpensesView)
            .Balance(balanceView)
            .gainings(fun _ -> if not (String.IsNullOrWhiteSpace(incomegroup.Value)) && gain.Value <> 0.0 then addIncome())
            .losings(fun _ -> if not (String.IsNullOrWhiteSpace(expensegroup.Value)) && loss.Value <> 0.0 then addExpenses())
            .Doc()


    module Stocks =
        open System
        type Stock = {
            Name: string
            Amount: float
            Price: float
            mutable LastPrice: float
            }

        let stockN, stockA, stockP  = Var.Create "", Var.Create 0.0, Var.Create 0.0
        let private random = Random()
        let private shufflePrice (price: float) =
            let randomFactor = (random.NextDouble() - 0.5) * 0.2
            let trendFactor = 1.0 + randomFactor
            let newPrice = price * trendFactor
            newPrice

        let initialStockData = [
            { Name = "Apple"; Amount = 1.1; Price = 189.0; LastPrice = shufflePrice 189.0 }
            { Name = "Alphabet"; Amount = 1.1; Price = 170.0; LastPrice = shufflePrice 170.0 }
            { Name = "Microsoft"; Amount = 1.3; Price = 416.0; LastPrice = shufflePrice 416.0 }
        ]

        let loadStockData () =
            match JS.Window.LocalStorage.GetItem("stocks") with
            | null -> initialStockData
            | data ->
                try
                    let stocks = Json.Deserialize<Stock list>(data)
                    stocks |> List.map (fun stock -> { stock with LastPrice = shufflePrice stock.Price })
                with
                | _ -> initialStockData

        let saveStockData (stocks: Stock list) =
            let data = Json.Serialize stocks
            JS.Window.LocalStorage.SetItem("stocks", data)

        let stockModel = 
            let stocks = loadStockData()
            let model = ListModel.Create (fun stock -> stock.Name) stocks
            model.View |> View.Sink (fun stocks -> saveStockData (List.ofSeq stocks))
            model

        let capitalize (str: string) =
            if String.IsNullOrWhiteSpace(str) then str
            else str.[0].ToString().ToUpper() + str.Substring(1).ToLower()

        let priceupdate() =
            stockModel.Iter(fun stock ->
                let newLastPrice = shufflePrice stock.Price
                stockModel.UpdateBy (fun s -> if s.Name = stock.Name then Some { s with LastPrice = newLastPrice } else None) stock.Name
            )

        let UpdateLatestPrices () =
            async {
                while true do
                    do! Async.Sleep 3000
                    priceupdate()
            }
            |> Async.Start


        let CounterPage() =
                UpdateLatestPrices()
                let totalAsset =
                    stockModel.View
                    |> View.Map (Seq.sumBy (fun stock -> stock.LastPrice * stock.Amount)
                    )
                let totalProfitAndLoss =
                    stockModel.View
                    |> View.Map (Seq.sumBy (fun stock -> (stock.LastPrice * stock.Amount) - (stock.Price * stock.Amount))
                )

                let addNewStock (stockN: Var<string>, stockA: Var<float>, stockP: Var<float>) =
                    if not <| String.IsNullOrWhiteSpace(stockN.Value) && stockA.Value > 0.0 && stockP.Value > 0.0 then
                        let newStock = {
                            Name = capitalize stockN.Value
                            Amount = stockA.Value
                            Price = stockP.Value
                            LastPrice = shufflePrice stockP.Value
                        }
                        stockModel.Add newStock
                        stockN.Value <- ""; stockA.Value <- 0.0; stockP.Value <- 0.0

                let getColor profitAndLoss = if profitAndLoss >= 0.0 then "rgb(0, 255, 0)" else "red"

                let renderProfitAndLossPercent totalAsset totalProfitAndLoss =
                    View.Map2 (fun asset profitAndLoss -> sprintf "%.2f" (profitAndLoss * 100.0 / asset)) totalAsset totalProfitAndLoss

                let renderProfitAndLossColor totalProfitAndLoss =
                    View.Map (fun profitAndLoss -> if profitAndLoss >= 0.0 then "rgb(0, 255, 0)" else "red") totalProfitAndLoss

                let calculateStockValues stock =
                    let costBasis = stock.Amount * stock.Price
                    let marketValue = stock.LastPrice * stock.Amount
                    let profitAndLoss = marketValue - costBasis
                    let print value = sprintf "%.2f" value
                    (costBasis, marketValue, profitAndLoss, print)

                IndexTemplate.StockPortfolio()
                        .stockTableBody(
                            stockModel.View.DocSeqCached(fun stock ->
                                let (costBasis, marketValue, profitAndLoss, print) = calculateStockValues stock
                                IndexTemplate.stockList()
                                    .stockName(stock.Name)
                                    .stockAmount(print stock.Amount)
                                    .stockPrice(print stock.Price)
                                    .stockLast(print stock.LastPrice)
                                    .color(getColor profitAndLoss)
                                    .stockProfitAndLoss(print profitAndLoss)
                                    .stockCostBasis(print costBasis)
                                    .stockMarketValue(print marketValue)
                                    .remove(fun _ -> stockModel.RemoveByKey stock.Name)
                                    .Doc()))
                        .stockName(stockN)
                        .stockAmount(stockA)
                        .stockPrice(stockP)
                        .assetAmount(totalAsset |> View.Map (sprintf "%.2f$"))
                        .assetProfitAndLoss(totalProfitAndLoss |> View.Map (sprintf "%.2f$"))
                        .assetProfitAndLossPercent(renderProfitAndLossPercent totalAsset totalProfitAndLoss)
                        .colorProfitAndLoss(renderProfitAndLossColor totalProfitAndLoss)
                        .add(fun _ -> addNewStock (stockN, stockA, stockP))
                        .Doc()

 [<JavaScript>]
                
module App =
    open WebSharper.UI.Notation

    // Create a router for our endpoints
    let router = Router.Infer<EndPoint>()
    // Install our client-side router and track the current page
    let currentPage = Router.InstallHash Home router

    type Router<'T when 'T: equality> with
        member this.LinkHash (ep: 'T) = "#" + this.Link ep

    [<SPAEntryPoint>]
    let Main () =
        let renderInnerPage (currentPage: Var<EndPoint>) =
            currentPage.View.Map (fun endpoint ->
                match endpoint with
                | Home      -> Pages.Homepage()
                | Counter   -> Pages.Stocks.CounterPage()
            )
            |> Doc.EmbedView
        
        IndexTemplate()
            .Url_Home(router.LinkHash EndPoint.Home)
            .Url_Page1(router.LinkHash EndPoint.Counter)
           
            .MainContainer(renderInnerPage currentPage)
            .Bind()