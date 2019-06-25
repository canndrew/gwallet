namespace GWallet.Frontend.XF

open System
open System.Linq
open System.Threading
open System.Threading.Tasks

open Xamarin.Forms
open Xamarin.Forms.Xaml
open Plugin.Connectivity

open GWallet.Backend


// this type allows us to represent the idea that if we have, for example, 3 LTC and an unknown number of ETC (might
// be because all ETC servers are unresponsive), then it means we have AT LEAST 3LTC; as opposed to when we know for
// sure all balances of all currencies because all servers are responsive
type TotalBalance =
    | ExactBalance of decimal
    | AtLeastBalance of decimal
    static member (+) (x: TotalBalance, y: decimal) =
        match x with
        | ExactBalance exactBalance -> ExactBalance (exactBalance + y)
        | AtLeastBalance exactBalance -> AtLeastBalance (exactBalance + y)
    static member (+) (x: decimal, y: TotalBalance) =
        y + x

type BalancesPage(normalAccountsAndBalances: seq<BalanceState>,
                  currencyImages: Map<Currency*bool,Image>)
                      as this =
    inherit ContentPage()

    let _ = base.LoadFromXaml(typeof<BalancesPage>)

    let mainLayout = base.FindByName<StackLayout>("mainLayout")
    let totalFiatAmountLabel = mainLayout.FindByName<Label> "totalFiatAmountLabel"
    let totalReadOnlyFiatAmountLabel = mainLayout.FindByName<Label> "totalReadOnlyFiatAmountLabel"
    let totalFiatAmountFrame = mainLayout.FindByName<Frame> "totalFiatAmountFrame"
    let totalReadOnlyFiatAmountFrame = mainLayout.FindByName<Frame> "totalReadOnlyFiatAmountFrame"
    let contentLayout = base.FindByName<StackLayout> "contentLayout"

    let standardTimeToRefreshBalances = TimeSpan.FromMinutes 5.0
    let standardTimeToRefreshBalancesWhenThereIsImminentIncomingPaymentOrNotEnoughInfoToKnow = TimeSpan.FromMinutes 1.0
    let timerStartDelay = TimeSpan.FromMilliseconds 500.

    // FIXME: should reuse code with FrontendHelpers.BalanceInUsdString
    let UpdateGlobalFiatBalanceLabel (balance: MaybeCached<TotalBalance>) (totalFiatAmountLabel: Label) =
        let strBalance =
            match balance with
            | NotFresh NotAvailable ->
                "? USD"
            | Fresh amount ->
                match amount with
                | ExactBalance exactAmount ->
                    sprintf "~ %s USD" (Formatting.DecimalAmountRounding CurrencyType.Fiat exactAmount)
                | AtLeastBalance atLeastAmount ->
                    sprintf "~ %s USD?" (Formatting.DecimalAmountRounding CurrencyType.Fiat atLeastAmount)
            | NotFresh(Cached(cachedAmount,time)) ->
                match cachedAmount with
                | ExactBalance exactAmount ->
                    sprintf "~ %s USD%s"
                           (Formatting.DecimalAmountRounding CurrencyType.Fiat exactAmount)
                           (FrontendHelpers.MaybeReturnOutdatedMarkForOldDate time)
                | AtLeastBalance atLeastAmount ->
                    sprintf "~ %s USD%s?"
                           (Formatting.DecimalAmountRounding CurrencyType.Fiat atLeastAmount)
                           (FrontendHelpers.MaybeReturnOutdatedMarkForOldDate time)

        totalFiatAmountLabel.Text <- sprintf "Total Assets:\n%s" strBalance

    let rec UpdateGlobalFiatBalance (acc: Option<MaybeCached<TotalBalance>>)
                                    (fiatBalances: List<MaybeCached<decimal>>)
                                    totalFiatAmountLabel
                                        : unit =
        let updateGlobalFiatBalanceFromFreshAcc accAmount head tail =
            match head with
            | NotFresh NotAvailable ->
                match accAmount with
                | ExactBalance exactAccAmount ->
                    UpdateGlobalFiatBalanceLabel (Fresh (AtLeastBalance exactAccAmount)) totalFiatAmountLabel
                | AtLeastBalance atLeastAccAmount ->
                    UpdateGlobalFiatBalanceLabel (Fresh (AtLeastBalance atLeastAccAmount)) totalFiatAmountLabel
            | Fresh newAmount ->
                UpdateGlobalFiatBalance (Some(Fresh (newAmount+accAmount))) tail totalFiatAmountLabel
            | NotFresh(Cached(newCachedAmount,time)) ->
                UpdateGlobalFiatBalance (Some(NotFresh(Cached(newCachedAmount+accAmount,time))))
                                        tail
                                        totalFiatAmountLabel

        match acc with
        | None ->
            match fiatBalances with
            | [] ->
                failwith "unexpected: accumulator should be Some(thing) or coming balances shouldn't be List.empty"
            | head::tail ->
                let accAmount = 0.0m
                updateGlobalFiatBalanceFromFreshAcc (ExactBalance(accAmount)) head tail
        | Some(NotFresh NotAvailable) ->
            UpdateGlobalFiatBalanceLabel (NotFresh(NotAvailable)) totalFiatAmountLabel
        | Some(Fresh accAmount) ->
            match fiatBalances with
            | [] ->
                UpdateGlobalFiatBalanceLabel (Fresh accAmount) totalFiatAmountLabel
            | head::tail ->
                updateGlobalFiatBalanceFromFreshAcc accAmount head tail
        | Some(NotFresh(Cached(cachedAccAmount,accTime))) ->
            match fiatBalances with
            | [] ->
                UpdateGlobalFiatBalanceLabel (NotFresh(Cached(cachedAccAmount,accTime))) totalFiatAmountLabel
            | head::tail ->
                match head with
                | NotFresh NotAvailable ->
                    match cachedAccAmount with
                    | ExactBalance exactAccAmount ->
                        UpdateGlobalFiatBalanceLabel (NotFresh(Cached(AtLeastBalance exactAccAmount,accTime)))
                                                     totalFiatAmountLabel
                    | AtLeastBalance atLeastAccAmount ->
                        UpdateGlobalFiatBalanceLabel (NotFresh(Cached(AtLeastBalance atLeastAccAmount,accTime)))
                                                     totalFiatAmountLabel
                | Fresh newAmount ->
                    UpdateGlobalFiatBalance (Some(NotFresh(Cached(newAmount+cachedAccAmount,accTime))))
                                            tail
                                            totalFiatAmountLabel
                | NotFresh(Cached(newCachedAmount,time)) ->
                    UpdateGlobalFiatBalance (Some(NotFresh(Cached(newCachedAmount+cachedAccAmount,min accTime time))))
                                            tail
                                            totalFiatAmountLabel

    let normalCryptoBalanceClassId = "normalCryptoBalanceFrame"
    let readonlyCryptoBalanceClassId = "readonlyCryptoBalanceFrame"

    let rec FindCryptoBalances (cryptoBalanceClassId: string) (layout: StackLayout) 
                               (elements: List<View>) (resultsSoFar: List<Frame>): List<Frame> =
        match elements with
        | [] -> resultsSoFar
        | head::tail ->
            match head with
            | :? Frame as frame ->
                let newResults =
                    if frame.ClassId = cryptoBalanceClassId then
                        frame::resultsSoFar
                    else
                        resultsSoFar
                FindCryptoBalances cryptoBalanceClassId layout tail newResults
            | _ ->
                FindCryptoBalances cryptoBalanceClassId layout tail resultsSoFar

    let GetAmountOrDefault maybeAmount =
        match maybeAmount with
        | NotFresh NotAvailable ->
            0m
        | Fresh amount | NotFresh (Cached (amount,_)) ->
            amount

    let GetBaseRefreshInterval() =
        if this.NoImminentIncomingPayment then
            standardTimeToRefreshBalances
        else
            standardTimeToRefreshBalancesWhenThereIsImminentIncomingPaymentOrNotEnoughInfoToKnow

    let mutable lastRefreshBalancesStamp = DateTime.UtcNow,new CancellationTokenSource()

    // default value of the below field is 'false', just in case there's an incoming payment which we don't want to miss
    let mutable noImminentIncomingPayment = false

    let mutable balanceRefreshCancelSources = Seq.empty

    let lockObject = Object()

    do
        this.Init()

    [<Obsolete(DummyPageConstructorHelper.Warning)>]
    new() = BalancesPage(Seq.empty,Map.empty)

    member private this.LastRefreshBalancesStamp
        with get() = lock lockObject (fun _ -> lastRefreshBalancesStamp)
        and set value = lock lockObject (fun _ -> lastRefreshBalancesStamp <- value)
        
    member private this.NoImminentIncomingPayment
        with get() = lock lockObject (fun _ -> noImminentIncomingPayment)
         and set value = lock lockObject (fun _ -> noImminentIncomingPayment <- value)

    member private this.BalanceRefreshCancelSources
        with get() = lock lockObject (fun _ -> balanceRefreshCancelSources |> List.ofSeq :> seq<_>)
         and set value = lock lockObject (fun _ -> balanceRefreshCancelSources <- value)

    member this.PopulateBalances (readOnly: bool) (balances: seq<BalanceState>) =
        let activeCurrencyClassId,inactiveCurrencyClassId =
            if readOnly then
                readonlyCryptoBalanceClassId,normalCryptoBalanceClassId
            else
                normalCryptoBalanceClassId,readonlyCryptoBalanceClassId

        let activeCryptoBalances = FindCryptoBalances activeCurrencyClassId 
                                                      contentLayout 
                                                      (contentLayout.Children |> List.ofSeq) 
                                                      List.Empty

        let inactiveCryptoBalances = FindCryptoBalances inactiveCurrencyClassId 
                                                        contentLayout 
                                                        (contentLayout.Children |> List.ofSeq) 
                                                        List.Empty

        contentLayout.BatchBegin()                      

        for inactiveCryptoBalance in inactiveCryptoBalances do
            inactiveCryptoBalance.IsVisible <- false

        //We should create new frames only once, then just play with IsVisible(True|False) 
        if activeCryptoBalances.Any() then
            for activeCryptoBalance in activeCryptoBalances do
                activeCryptoBalance.IsVisible <- true
        else
            for balanceState in balances do
                let tapGestureRecognizer = TapGestureRecognizer()
                tapGestureRecognizer.Tapped.Subscribe(fun _ ->
                    let receivePage =
                        ReceivePage(balanceState.BalanceSet.Account, this,
                                    balanceState.BalanceSet.CryptoLabel, balanceState.BalanceSet.FiatLabel)
                    NavigationPage.SetHasNavigationBar(receivePage, false)
                    let navPage = NavigationPage receivePage

                    this.Navigation.PushAsync navPage
                         |> FrontendHelpers.DoubleCheckCompletionNonGeneric
                ) |> ignore

                let colorBoxWidth = 10.

                let stackLayout = StackLayout(Orientation = StackOrientation.Horizontal,
                                              Padding = Thickness(20., 20., colorBoxWidth + 10., 20.))

                let colour =
                    if readOnly then
                        "grey"
                    else
                        "red"

                let currencyLogoImg = currencyImages.[(balanceState.BalanceSet.Account.Currency,readOnly)]
                let cryptoLabel = balanceState.BalanceSet.CryptoLabel
                let fiatLabel = balanceState.BalanceSet.FiatLabel

                stackLayout.Children.Add currencyLogoImg
                stackLayout.Children.Add cryptoLabel
                stackLayout.Children.Add fiatLabel

                let colorBox = BoxView(Color = FrontendHelpers.GetCryptoColor balanceState.BalanceSet.Account.Currency)

                let absoluteLayout = AbsoluteLayout(Margin = Thickness(0., 1., 3., 1.))
                absoluteLayout.Children.Add(stackLayout, Rectangle(0., 0., 1., 1.), AbsoluteLayoutFlags.All)
                absoluteLayout.Children.Add(colorBox, Rectangle(1., 0., colorBoxWidth, 1.), AbsoluteLayoutFlags.PositionProportional ||| AbsoluteLayoutFlags.HeightProportional)

                if Device.RuntimePlatform = Device.GTK then
                    //workaround about GTK ScrollView's scroll bar. Not sure if it's bug indeed.
                    absoluteLayout.Margin <- Thickness(absoluteLayout.Margin.Left, absoluteLayout.Margin.Top, 20., absoluteLayout.Margin.Bottom)
                    //workaround about GTK layouting. It ignores margins of parent layout. So, we have to duplicate them
                    stackLayout.Margin <- Thickness(stackLayout.Margin.Left, stackLayout.Margin.Top, 20., stackLayout.Margin.Bottom)

                //TODO: remove this workaround once https://github.com/xamarin/Xamarin.Forms/pull/5207 is merged
                if Device.RuntimePlatform = Device.macOS then
                    let bindImageSize bindableProperty =
                        let binding = Binding(Path = "Height", Source = cryptoLabel)
                        currencyLogoImg.SetBinding(bindableProperty, binding)

                    bindImageSize VisualElement.WidthRequestProperty
                    bindImageSize VisualElement.HeightRequestProperty


                let frame = Frame(HasShadow = false,
                                  ClassId = activeCurrencyClassId,
                                  Content = absoluteLayout,
                                  Padding = Thickness(0.),
                                  BorderColor = Color.SeaShell)
                frame.GestureRecognizers.Add tapGestureRecognizer

                contentLayout.Children.Add frame

        contentLayout.BatchCommit()

    member this.UpdateGlobalFiatBalanceSum (allFiatBalances: seq<MaybeCached<decimal>>) totalFiatAmountLabel =
        let fiatBalancesList = allFiatBalances |> List.ofSeq
        if fiatBalancesList.Any() then
            UpdateGlobalFiatBalance None fiatBalancesList totalFiatAmountLabel

    member private this.UpdateGlobalBalance (state: FrontendHelpers.IGlobalAppState)
                                            (balancesJob: Async<array<BalanceState>>)
                                            fiatLabel
                                            donutView
                                                : Async<Option<bool>> =
        async {
            let _,cancelSource = this.LastRefreshBalancesStamp
            if cancelSource.IsCancellationRequested then

                // as in: we can't(NONE) know the answer to this because we're going to sleep
                return None

            else
                let! resolvedBalances = balancesJob
                let fiatBalances = resolvedBalances.Select(fun balanceState ->
                                                                     balanceState.FiatAmount)
                Device.BeginInvokeOnMainThread(fun _ ->
                    this.UpdateGlobalFiatBalanceSum fiatBalances fiatLabel
                )
                return resolvedBalances.Any(fun balanceState ->

                    // ".IsNone" means: we don't know if there's an incoming payment (absence of info)
                    // so the whole `".IsNone" || "yes"` means: maybe there's an imminent incoming payment?
                    // as in "it's false that we know for sure that there's no incoming payment"
                    balanceState.ImminentIncomingPayment.IsNone ||
                        Option.exists id balanceState.ImminentIncomingPayment

                // we can(SOME) answer: either there's no incoming payment (false) or that maybe there is (true)
                ) |> Some
        }

    member this.PopulateGrid () =
        this.PopulateBalances false normalAccountsAndBalances

    member private this.Init () =
        FrontendHelpers.ApplyGtkWorkaroundForFrameTransparentBackgroundColor totalFiatAmountFrame
        FrontendHelpers.ApplyGtkWorkaroundForFrameTransparentBackgroundColor totalReadOnlyFiatAmountFrame

        let tapGestureRecognizer = TapGestureRecognizer()
        tapGestureRecognizer.Tapped.Subscribe(fun _ ->
            Device.OpenUri (Uri "https://www.geewallet.com")
        ) |> ignore
        let footerLabel = mainLayout.FindByName<Label> "footerLabel"
        footerLabel.GestureRecognizers.Add tapGestureRecognizer

        let allNormalAccountFiatBalances =
            normalAccountsAndBalances.Select(fun balanceState -> balanceState.FiatAmount) |> List.ofSeq

        Device.BeginInvokeOnMainThread(fun _ ->
            this.PopulateGrid ()

            this.UpdateGlobalFiatBalanceSum allNormalAccountFiatBalances totalFiatAmountLabel
        )
