﻿namespace GWallet.Backend.Tests

open System
open System.IO

open NUnit.Framework

open GWallet.Backend

[<TestFixture>]
type CompoundBalanceCaching() =

    let high_expiration_span_because_this_test_doesnt_involve_timing = TimeSpan.FromDays 100.0
    let zero_fee_because_this_test_does_not_involve_fees = 0.0m

    let someAddress = "0xABC"
    let someCurrency = Currency.ETC
    let someDummyTxId = "x"
    let someSameFeeCurrency = someCurrency

    let SpawnNewCacheInstanceToTest(expirationSpan: TimeSpan) =
        let tempFile = Path.GetTempFileName() |> FileInfo
        Caching.MainCache(Some tempFile, expirationSpan),tempFile

    [<Test>]
    member __.``combinations metatest``() =
        let someMap: Map<string,int> = Map.empty.Add("x", 1).Add("y", 2).Add("z", 3)
        let combinations = Caching.MapCombinations someMap
        Assert.That(combinations.Length, Is.EqualTo 7)

    [<Test>]
    member __.``non-compound balance``() =
        let cache,cacheFile =
            SpawnNewCacheInstanceToTest high_expiration_span_because_this_test_doesnt_involve_timing

        try
            let someBalance = 10m
            let cachedBalance,_ = cache.RetreiveAndUpdateLastCompoundBalance someAddress someCurrency someBalance
            Assert.That(cachedBalance, Is.EqualTo someBalance)
        finally
            File.Delete cacheFile.FullName

    [<Test>]
    member __.``single-compound balance``() =
        let cache,cacheFile =
            SpawnNewCacheInstanceToTest high_expiration_span_because_this_test_doesnt_involve_timing

        try
            let someBalance = 10m
            let cachedBalance,_ = cache.RetreiveAndUpdateLastCompoundBalance someAddress someCurrency someBalance
            Assert.That(cachedBalance, Is.EqualTo someBalance)

            let someTransactionValue = 1m
            cache.StoreOutgoingTransaction someAddress
                                           someCurrency
                                           someSameFeeCurrency
                                           someDummyTxId
                                           someTransactionValue
                                           zero_fee_because_this_test_does_not_involve_fees
            match cache.RetreiveLastCompoundBalance someAddress someCurrency with
            | NotAvailable -> Assert.Fail "should have saved some balance"
            | Cached(cachedBalance2,_) ->
                Assert.That(cachedBalance2, Is.EqualTo (someBalance - someTransactionValue))
        finally
            File.Delete cacheFile.FullName

    [<Test>]
    member __.``single-compound balance with dupe tx``() =
        let cache,cacheFile =
            SpawnNewCacheInstanceToTest high_expiration_span_because_this_test_doesnt_involve_timing

        try
            let someBalance = 10m
            let cachedBalance,_ = cache.RetreiveAndUpdateLastCompoundBalance someAddress someCurrency someBalance
            Assert.That(cachedBalance, Is.EqualTo someBalance)

            let someTransactionValue = 1m
            cache.StoreOutgoingTransaction someAddress
                                           someCurrency
                                           someSameFeeCurrency
                                           someDummyTxId
                                           someTransactionValue
                                           zero_fee_because_this_test_does_not_involve_fees
            cache.StoreOutgoingTransaction someAddress
                                           someCurrency
                                           someSameFeeCurrency
                                           someDummyTxId
                                           someTransactionValue
                                           zero_fee_because_this_test_does_not_involve_fees

            match cache.RetreiveLastCompoundBalance someAddress someCurrency with
            | NotAvailable -> Assert.Fail "should have saved some balance"
            | Cached(cachedBalance2,_) ->
                Assert.That(cachedBalance2, Is.EqualTo (someBalance - someTransactionValue))
        finally
            File.Delete cacheFile.FullName

    [<Test>]
    member __.``double-compound balance``() =
        let cache,cacheFile =
            SpawnNewCacheInstanceToTest high_expiration_span_because_this_test_doesnt_involve_timing

        try
            let someBalance = 10m
            let cachedBalance,_ = cache.RetreiveAndUpdateLastCompoundBalance someAddress someCurrency someBalance
            Assert.That(cachedBalance, Is.EqualTo someBalance)

            let firstTransactionAmount = 1m
            cache.StoreOutgoingTransaction someAddress
                                           someCurrency
                                           someSameFeeCurrency
                                           "x"
                                           firstTransactionAmount
                                           zero_fee_because_this_test_does_not_involve_fees
            let secondTransactionAmount = 2m
            cache.StoreOutgoingTransaction someAddress
                                           someCurrency
                                           someSameFeeCurrency
                                           "y"
                                           secondTransactionAmount
                                           zero_fee_because_this_test_does_not_involve_fees

            match cache.RetreiveLastCompoundBalance someAddress someCurrency with
            | NotAvailable -> Assert.Fail "should have saved some balance"
            | Cached(cachedBalance2,_) ->
                Assert.That(cachedBalance2, Is.EqualTo (someBalance - firstTransactionAmount - secondTransactionAmount))
        finally
            File.Delete cacheFile.FullName

    [<Test>]
    member __.``confirmed first transaction``() =
        let cache,cacheFile =
            SpawnNewCacheInstanceToTest high_expiration_span_because_this_test_doesnt_involve_timing

        try
            let someBalance = 10m
            let cachedBalance,_ = cache.RetreiveAndUpdateLastCompoundBalance someAddress someCurrency someBalance
            Assert.That(cachedBalance, Is.EqualTo someBalance)

            let firstTransactionAmount = 1m
            cache.StoreOutgoingTransaction someAddress
                                           someCurrency
                                           someSameFeeCurrency
                                           "x"
                                           firstTransactionAmount
                                           zero_fee_because_this_test_does_not_involve_fees
            let secondTransactionAmount = 2m
            cache.StoreOutgoingTransaction someAddress
                                           someCurrency
                                           someSameFeeCurrency
                                           "y"
                                           secondTransactionAmount
                                           zero_fee_because_this_test_does_not_involve_fees

            let newBalanceAfterFirstTransactionIsConfirmed = someBalance - firstTransactionAmount
            let cachedBalance2,_ = cache.RetreiveAndUpdateLastCompoundBalance someAddress
                                                                              someCurrency
                                                                              newBalanceAfterFirstTransactionIsConfirmed
            Assert.That(cachedBalance2, Is.EqualTo (someBalance - firstTransactionAmount - secondTransactionAmount))
        finally
            File.Delete cacheFile.FullName

    [<Test>]
    member __.``confirmed second transaction``() =
        let cache,cacheFile =
            SpawnNewCacheInstanceToTest high_expiration_span_because_this_test_doesnt_involve_timing

        try
            let someBalance = 10m
            let cachedBalance,_ = cache.RetreiveAndUpdateLastCompoundBalance someAddress someCurrency someBalance
            Assert.That(cachedBalance, Is.EqualTo someBalance)

            let firstTransactionAmount = 1m
            cache.StoreOutgoingTransaction someAddress
                                           someCurrency
                                           someSameFeeCurrency
                                           "x"
                                           firstTransactionAmount
                                           zero_fee_because_this_test_does_not_involve_fees
            let cachedBalance = cache.RetreiveAndUpdateLastCompoundBalance someAddress someCurrency someBalance
            let secondTransactionAmount = 2m
            cache.StoreOutgoingTransaction someAddress
                                           someCurrency
                                           someSameFeeCurrency
                                           "y"
                                           secondTransactionAmount
                                           zero_fee_because_this_test_does_not_involve_fees

            let newBalanceAfterSndTransactionIsConfirmed = someBalance - secondTransactionAmount
            let cachedBalance2,_ = cache.RetreiveAndUpdateLastCompoundBalance someAddress
                                                                              someCurrency
                                                                              newBalanceAfterSndTransactionIsConfirmed
            Assert.That(cachedBalance2, Is.EqualTo (someBalance - firstTransactionAmount - secondTransactionAmount))
        finally
            File.Delete cacheFile.FullName

    [<Test>]
    member __.``confirmed two transactions``() =
        let cache,cacheFile =
            SpawnNewCacheInstanceToTest high_expiration_span_because_this_test_doesnt_involve_timing

        try
            let someBalance = 10m
            let cachedBalance,_ = cache.RetreiveAndUpdateLastCompoundBalance someAddress someCurrency someBalance
            Assert.That(cachedBalance, Is.EqualTo someBalance)

            let firstTransactionAmount = 1m
            cache.StoreOutgoingTransaction someAddress
                                           someCurrency
                                           someSameFeeCurrency
                                           "x"
                                           firstTransactionAmount
                                           zero_fee_because_this_test_does_not_involve_fees
            let secondTransactionAmount = 2m
            cache.StoreOutgoingTransaction someAddress
                                           someCurrency
                                           someSameFeeCurrency
                                           "y"
                                           secondTransactionAmount
                                           zero_fee_because_this_test_does_not_involve_fees

            let newBalanceAfterBothTxsAreConfirmed = someBalance - secondTransactionAmount - firstTransactionAmount
            let cachedBalance2,_ = cache.RetreiveAndUpdateLastCompoundBalance someAddress
                                                                              someCurrency
                                                                              newBalanceAfterBothTxsAreConfirmed
            Assert.That(cachedBalance2, Is.EqualTo (someBalance - firstTransactionAmount - secondTransactionAmount))
        finally
            File.Delete cacheFile.FullName

    [<Test>]
    member __.``single-compound balance with expired transaction (retrieve and update)``() =
        let expirationTime = TimeSpan.FromMilliseconds 100.0
        let cache,cacheFile =
            SpawnNewCacheInstanceToTest expirationTime

        try
            let someBalance = 10m
            let cachedBalance,_ = cache.RetreiveAndUpdateLastCompoundBalance someAddress someCurrency someBalance
            Assert.That(cachedBalance, Is.EqualTo someBalance)

            cache.StoreOutgoingTransaction someAddress
                                           someCurrency
                                           someSameFeeCurrency
                                           someDummyTxId
                                           1m
                                           zero_fee_because_this_test_does_not_involve_fees
            Threading.Thread.Sleep(expirationTime + expirationTime)
            let cachedBalance2,_ = cache.RetreiveAndUpdateLastCompoundBalance someAddress someCurrency someBalance
            Assert.That(cachedBalance2, Is.EqualTo someBalance)
        finally
            File.Delete cacheFile.FullName

    [<Test>]
    member __.``single-compound balance with expired transaction (just retreive)``() =
        let expirationTime = TimeSpan.FromMilliseconds 100.0
        let cache,cacheFile =
            SpawnNewCacheInstanceToTest expirationTime

        try
            let someBalance = 10m
            let cachedBalance,_ = cache.RetreiveAndUpdateLastCompoundBalance someAddress someCurrency someBalance
            Assert.That(cachedBalance, Is.EqualTo someBalance)

            cache.StoreOutgoingTransaction someAddress
                                           someCurrency
                                           someSameFeeCurrency
                                           someDummyTxId
                                           1m
                                           zero_fee_because_this_test_does_not_involve_fees
            Threading.Thread.Sleep(expirationTime + expirationTime)
            match cache.RetreiveLastCompoundBalance someAddress someCurrency with
            | NotAvailable -> Assert.Fail "should have saved some balance"
            | Cached(cachedBalance2,_) ->
                Assert.That(cachedBalance2, Is.EqualTo someBalance)
        finally
            File.Delete cacheFile.FullName

    [<Test>]
    member __.``substracting both currency X(e.g. DAI) and the currency Y(e.g.ETH) where fees are spent``() =
        let cache,cacheFile =
            SpawnNewCacheInstanceToTest high_expiration_span_because_this_test_doesnt_involve_timing

        try
            let someTokenCurrency = Currency.DAI
            let someTokenBalance = 10m
            let someEthCurrency = Currency.ETH
            let someEthBalance = 5m
            let cachedTokenBalance,_ =
                cache.RetreiveAndUpdateLastCompoundBalance someAddress someTokenCurrency someTokenBalance
            Assert.That(cachedTokenBalance, Is.EqualTo someTokenBalance)
            let cachedEthBalance,_ =
                cache.RetreiveAndUpdateLastCompoundBalance someAddress someEthCurrency someEthBalance
            Assert.That(cachedEthBalance, Is.EqualTo someEthBalance)

            let someTransactionAmount = 1m
            let someFeeAmount = 0.1m
            cache.StoreOutgoingTransaction someAddress
                                           someTokenCurrency
                                           someEthCurrency
                                           someDummyTxId
                                           someTransactionAmount
                                           someFeeAmount
            match cache.RetreiveLastCompoundBalance someAddress someTokenCurrency with
            | NotAvailable -> Assert.Fail "should have saved some balance"
            | Cached(cachedTokenBalance2,_) ->
                Assert.That(cachedTokenBalance2, Is.EqualTo (someTokenBalance - someTransactionAmount))

            match cache.RetreiveLastCompoundBalance someAddress someEthCurrency with
            | NotAvailable -> Assert.Fail "should have saved some balance"
            | Cached(cachedEthBalance2,_) ->
                if not Config.EthTokenEstimationCouldBeBuggyAsInNotAccurate then
                    Assert.That(cachedEthBalance2, Is.EqualTo (someEthBalance - someFeeAmount))
        finally
            File.Delete cacheFile.FullName
