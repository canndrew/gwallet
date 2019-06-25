﻿namespace GWallet.Frontend.XF

open System
open System.Threading.Tasks

open Xamarin.Forms
open Xamarin.Forms.Xaml

open Plugin.Clipboard
open Plugin.Connectivity
open ZXing
open ZXing.Net.Mobile.Forms
open ZXing.Common

open GWallet.Backend

type ReceivePage(account: IAccount,
                 balancesPage: Page,
                 cryptoBalanceLabelInBalancesPage: Label,
                 fiatBalanceLabelInBalancesPage: Label) as this =
    inherit ContentPage()
    let _ = base.LoadFromXaml(typeof<ReceivePage>)

    let mainLayout = base.FindByName<StackLayout>("mainLayout")

    let paymentCaption = "Send Payment"
    let paymentCaptionInColdStorage = "Signoff Payment Offline"

    do
        this.Init()

    [<Obsolete(DummyPageConstructorHelper.Warning)>]
    new() = ReceivePage(ReadOnlyAccount(Currency.BTC, { Name = "dummy"; Content = fun _ -> "" }, fun _ -> ""),
                        DummyPageConstructorHelper.PageFuncToRaiseExceptionIfUsedAtRuntime(),null,null)

    member this.Init() =
        let balanceLabel = mainLayout.FindByName<Label>("balanceLabel")
        let fiatBalanceLabel = mainLayout.FindByName<Label>("fiatBalanceLabel")

        let accountBalance =
            Caching.Instance.RetreiveLastCompoundBalance account.PublicAddress account.Currency
        FrontendHelpers.UpdateBalance (NotFresh accountBalance) account.Currency balanceLabel fiatBalanceLabel
            |> ignore

        // this below is for the case when a new ReceivePage() instance is suddenly created after sending a transaction
        // (we need to update the balance page ASAP in case the user goes back to it after sending the transaction)
        FrontendHelpers.UpdateBalance (NotFresh accountBalance)
                                      account.Currency
                                      cryptoBalanceLabelInBalancesPage
                                      fiatBalanceLabelInBalancesPage
            |> ignore

        balanceLabel.FontSize <- FrontendHelpers.BigFontSize
        fiatBalanceLabel.FontSize <- FrontendHelpers.MediumFontSize

        let size = 200
        let encodingOptions = EncodingOptions(Height = size,
                                              Width = size)
        let barCode = ZXingBarcodeImageView(HorizontalOptions = LayoutOptions.Center,
                                            VerticalOptions = LayoutOptions.Center,
                                            BarcodeFormat = BarcodeFormat.QR_CODE,
                                            BarcodeValue = account.PublicAddress,
                                            HeightRequest = float size,
                                            WidthRequest = float size,
                                            BarcodeOptions = encodingOptions)
        mainLayout.Children.Add(barCode)

        let transactionHistoryButton = Button(Text = "View transaction history...")
        transactionHistoryButton.Clicked.Subscribe(fun _ ->
            Device.OpenUri (BlockExplorer.GetTransactionHistory account)
        ) |> ignore
        mainLayout.Children.Add(transactionHistoryButton)

        // FIXME: report this Xamarin.Forms Mac backend bug (no back button in navigation pages!, so below <workaround>)
        if (Device.RuntimePlatform <> Device.macOS) then () else

        let backButton = Button(Text = "< Go back")
        backButton.Clicked.Subscribe(fun _ ->
            Device.BeginInvokeOnMainThread(fun _ ->
                balancesPage.Navigation.PopAsync() |> FrontendHelpers.DoubleCheckCompletion
            )
        ) |> ignore
        mainLayout.Children.Add(backButton)
        //</workaround>

    member this.OnCopyToClipboardClicked(sender: Object, args: EventArgs) =
        let copyToClipboardButton = base.FindByName<Button>("copyToClipboardButton")
        FrontendHelpers.ChangeTextAndChangeBack copyToClipboardButton "Copied"

        CrossClipboard.Current.SetText account.PublicAddress
        ()
