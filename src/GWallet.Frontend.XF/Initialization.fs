namespace GWallet.Frontend.XF

open System.Linq

open Xamarin.Forms

open GWallet.Backend

module Initialization =

    let internal GlobalState = GlobalState()

    let private GlobalInit () =
        Infrastructure.SetupSentryHook ()

    let CreateImage (currency: Currency) (readOnly: bool) =
        let colour =
            if readOnly then
                "grey"
            else
                "red"
        let currencyLowerCase = currency.ToString().ToLower()
        let imageSource = FrontendHelpers.GetSizedColoredImageSource currencyLowerCase colour 60
        let currencyLogoImg = Image(Source = imageSource, IsVisible = true)
        currencyLogoImg
    let GetAllCurrencyCases(): seq<Currency*bool> =
        seq {
            for currency in Currency.GetAll() do
                yield currency,true
                yield currency,false
        }
    let GetAllImages(): seq<(Currency*bool)*Image> =
        seq {
            for currency,readOnly in GetAllCurrencyCases() do
                yield (currency,readOnly),(CreateImage currency readOnly)
        }
    let PreLoadCurrencyImages(): Async<Map<Currency*bool,Image>> =
        async {
            return (GetAllImages() |> Map.ofSeq)
        }

    let internal LandingPage(): Async<NavigationPage> =
        let state = GlobalInit ()

        let accounts = Account.GetAllActiveAccounts()

        if not (accounts.Any()) then
            failwith "create accounts first (with Frontend.Console)"

        let normalAccountsBalances = FrontendHelpers.CreateWidgetsForAccounts accounts
        let _,allNormalAccountBalancesJob = FrontendHelpers.UpdateBalancesAsync normalAccountsBalances true Mode.Fast

        let preloadCurrencyImagesJob = PreLoadCurrencyImages()

        let populateGrid = async {
            let all3Jobs = FSharpUtil.AsyncExtensions.MixedParallel2 allNormalAccountBalancesJob
                                                                     preloadCurrencyImagesJob
            let! allResolvedNormalAccountBalances,currencyImages = all3Jobs

            let balancesPage = BalancesPage(allResolvedNormalAccountBalances,
                                            currencyImages)
            let navPage = NavigationPage balancesPage
            NavigationPage.SetHasNavigationBar(balancesPage, false)
            return navPage
        }
        populateGrid
