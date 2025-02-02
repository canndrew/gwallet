﻿namespace GWallet.Backend.Ether

open System
open System.Net
open System.Numerics
open System.Linq
open System.Threading

open Nethereum.Util
open Nethereum.Hex.HexTypes
open Nethereum.Web3
open Nethereum.RPC.Eth.DTOs
open Nethereum.StandardTokenEIP20.ContractDefinition

open GWallet.Backend

type BalanceType =
    | Unconfirmed
    | Confirmed

type SomeWeb3(url: string) =
    inherit Web3(url)

    member val Url = url with get

type TransactionStatusDetails =
    {
        GasUsed: BigInteger
        Status: BigInteger
    }

module Web3ServerSeedList =

    //let private PUBLIC_WEB3_API_ETH_INFURA = "https://mainnet.infura.io:8545" ?
    let private ethWeb3InfuraMyCrypto = SomeWeb3("https://mainnet.infura.io/mycrypto")
    let private ethWeb3InfuraMyCryptoV3 = SomeWeb3 "https://mainnet.infura.io/v3/c02fff6b5daa434d8422b8ece54c7286"
    let private ethWeb3Mew = SomeWeb3("https://api.myetherapi.com/eth") // docs: https://www.myetherapi.com/
    let private ethWeb3Giveth = SomeWeb3("https://mew.giveth.io")
    let private ethMyCrypto = SomeWeb3("https://api.mycryptoapi.com/eth")
    let private ethBlockScale = SomeWeb3("https://api.dev.blockscale.net/dev/parity")
    let private ethWeb3InfuraMyEtherWallet = SomeWeb3("https://mainnet.infura.io/mew")
    let private ethWeb3MewAws = SomeWeb3 "https://o70075sme1.execute-api.us-east-1.amazonaws.com/latest/eth"
    let private ethAlchemyApi = SomeWeb3 "https://eth-mainnet.alchemyapi.io/jsonrpc/-vPGIFwUyjlMRF9beTLXiGQUK6Nf3k8z"
    // not sure why the below one doesn't work, gives some JSON error
    //let private ethWeb3EtherScan = SomeWeb3 "https://api.etherscan.io/api"

    // TODO: add the one from https://etcchain.com/api/ too
    let private etcWeb3ePoolIo1 = SomeWeb3("https://cry.epool.io")
    let private etcWeb3ePoolIo2 = SomeWeb3("https://mew.epool.io")
    let private etcWeb3ePoolIo3 = SomeWeb3("https://mewapi.epool.io")
    let private etcWeb3ZeroXInfraGeth = SomeWeb3("https://etc-geth.0xinfra.com")
    let private etcWeb3ZeroXInfraParity = SomeWeb3("https://etc-parity.0xinfra.com")
    let private etcWeb3CommonWealthGeth = SomeWeb3("https://etcrpc.viperid.online")
    // FIXME: the below one doesn't seem to work; we should include it anyway and make the algorithm discard it at runtime
    //let private etcWeb3CommonWealthMantis = SomeWeb3("https://etc-mantis.callisto.network")
    let private etcWeb3CommonWealthParity = SomeWeb3("https://etc-parity.callisto.network")
    let private etcWeb3ChainKorea = SomeWeb3("https://node.classicexplorer.org/")
    let private etcWeb3GasTracker = SomeWeb3 "https://web3.gastracker.io"
    let private etcWeb3EtcCooperative = SomeWeb3 "https://ethereumclassic.network"

    let private GetWeb3Servers (currency: Currency): List<SomeWeb3> =
        if currency = ETC then
            [
                etcWeb3EtcCooperative;
                etcWeb3GasTracker;
                etcWeb3ePoolIo1;
                etcWeb3ChainKorea;
                etcWeb3CommonWealthParity;
                etcWeb3CommonWealthGeth;
                etcWeb3ZeroXInfraParity;
                etcWeb3ZeroXInfraGeth;
                etcWeb3ePoolIo2;
                etcWeb3ePoolIo3;
            ]
        elif (currency.IsEthToken() || currency = Currency.ETH) then
            [
                ethWeb3MewAws;
                ethWeb3InfuraMyCrypto;
                ethWeb3InfuraMyCryptoV3
                ethWeb3Mew;
                ethWeb3Giveth;
                ethMyCrypto;
                ethBlockScale;
                ethWeb3InfuraMyEtherWallet;
                ethAlchemyApi
            ]
        else
            failwithf "Assertion failed: Ether currency %A not supported?" currency

    let Randomize currency =
        let serverList = GetWeb3Servers currency
        Shuffler.Unsort serverList


module Server =

    let HttpRequestExceptionMatchesErrorCode (ex: Http.HttpRequestException) (errorCode: int): bool =
        ex.Message.StartsWith(sprintf "%d " errorCode) || ex.Message.Contains(sprintf " %d " errorCode)

    let exMsg = "Could not communicate with EtherServer"
    let PerformEtherRemoteCallWithTimeout<'T,'R> (job: Async<'R>): Async<'R> = async {
        let! maybeResult = FSharpUtil.WithTimeout Config.DEFAULT_NETWORK_TIMEOUT job
        match maybeResult with
        | None ->
            return raise <| ServerTimedOutException("Timeout when trying to communicate with Ether server")
        | Some result ->
            return result
    }

    let MaybeRethrowHttpRequestException (ex: Exception): unit =
        let maybeHttpReqEx = FSharpUtil.FindException<Http.HttpRequestException> ex
        match maybeHttpReqEx with
        | Some httpReqEx ->
            if HttpRequestExceptionMatchesErrorCode httpReqEx (int CloudFlareError.ConnectionTimeOut) then
                raise <| ServerTimedOutException(exMsg, httpReqEx)
            if HttpRequestExceptionMatchesErrorCode httpReqEx (int CloudFlareError.OriginUnreachable) then
                raise <| ServerTimedOutException(exMsg, httpReqEx)

            if HttpRequestExceptionMatchesErrorCode httpReqEx (int CloudFlareError.OriginSslHandshakeError) then
                raise <| ServerChannelNegotiationException(exMsg, CloudFlareError.OriginSslHandshakeError, httpReqEx)

            if HttpRequestExceptionMatchesErrorCode httpReqEx (int CloudFlareError.WebServerDown) then
                raise <| ServerUnreachableException(exMsg, CloudFlareError.WebServerDown, httpReqEx)
            if HttpRequestExceptionMatchesErrorCode httpReqEx (int HttpStatusCode.BadGateway) then
                raise <| ServerUnreachableException(exMsg, HttpStatusCode.BadGateway, httpReqEx)
            if HttpRequestExceptionMatchesErrorCode httpReqEx (int HttpStatusCode.GatewayTimeout) then
                raise <| ServerUnreachableException(exMsg, HttpStatusCode.GatewayTimeout, httpReqEx)

            if HttpRequestExceptionMatchesErrorCode httpReqEx (int HttpStatusCode.ServiceUnavailable) then
                raise <| ServerUnavailableException(exMsg, httpReqEx)

            // TODO: maybe in these cases below, blacklist the server somehow if it keeps giving this error:
            if HttpRequestExceptionMatchesErrorCode httpReqEx (int HttpStatusCode.Forbidden) then
                raise <| ServerMisconfiguredException(exMsg, httpReqEx)
            if HttpRequestExceptionMatchesErrorCode httpReqEx (int HttpStatusCode.MethodNotAllowed) then
                raise <| ServerMisconfiguredException(exMsg, httpReqEx)
            if HttpRequestExceptionMatchesErrorCode httpReqEx (int HttpStatusCode.InternalServerError) then
                raise <| ServerUnavailableException(exMsg, httpReqEx)

            if HttpRequestExceptionMatchesErrorCode
                httpReqEx (int HttpStatusCodeNotPresentInTheBcl.TooManyRequests) then
                    raise <| ServerRestrictiveException(exMsg, httpReqEx)

        | None ->
            ()

    let MaybeRethrowRpcResponseException (ex: Exception): unit =
        let maybeRpcResponseEx = FSharpUtil.FindException<JsonRpcSharp.Client.RpcResponseException> ex
        match maybeRpcResponseEx with
        | Some rpcResponseEx ->
            if rpcResponseEx.RpcError <> null then
                if rpcResponseEx.RpcError.Code = int RpcErrorCode.StatePruningNode then
                    if not (rpcResponseEx.RpcError.Message.Contains("pruning=archive")) then
                        raise <| Exception(sprintf "Expecting 'pruning=archive' in message of a %d code"
                                                   (int RpcErrorCode.StatePruningNode), rpcResponseEx)
                    else
                        raise <| ServerMisconfiguredException(exMsg, rpcResponseEx)
                if (rpcResponseEx.RpcError.Code = int RpcErrorCode.UnknownBlockNumber) then
                    raise <| ServerMisconfiguredException(exMsg, rpcResponseEx)
                if rpcResponseEx.RpcError.Code = int RpcErrorCode.GatewayTimeout then
                    raise <| ServerMisconfiguredException(exMsg, rpcResponseEx)
                raise <| Exception(sprintf "RpcResponseException with RpcError Code %d and Message %s (%s)"
                                         rpcResponseEx.RpcError.Code
                                         rpcResponseEx.RpcError.Message
                                         rpcResponseEx.Message,
                                   rpcResponseEx)
        | None ->
            ()

    let MaybeRethrowRpcClientTimeoutException (ex: Exception): unit =
        let maybeRpcTimeoutException =
            FSharpUtil.FindException<JsonRpcSharp.Client.RpcClientTimeoutException> ex
        match maybeRpcTimeoutException with
        | Some rpcTimeoutEx ->
            raise <| ServerTimedOutException(exMsg, rpcTimeoutEx)
        | None ->
            ()

    let MaybeRethrowNetworkingException (ex: Exception): unit =
        let maybeSocketRewrappedException = Networking.FindExceptionToRethrow ex exMsg
        match maybeSocketRewrappedException with
        | Some socketRewrappedException ->
            raise socketRewrappedException
        | None ->
            ()

    // this could be a Xamarin.Android bug (see https://gitlab.com/knocte/geewallet/issues/119)
    let MaybeRethrowObjectDisposedException (ex: Exception): unit =
        let maybeRpcUnknownEx = FSharpUtil.FindException<JsonRpcSharp.Client.RpcClientUnknownException> ex
        match maybeRpcUnknownEx with
        | Some rpcUnknownEx ->
            let maybeObjectDisposedEx = FSharpUtil.FindException<ObjectDisposedException> ex
            match maybeObjectDisposedEx with
            | Some objectDisposedEx ->
                if objectDisposedEx.Message.Contains "MobileAuthenticatedStream" then
                    raise <| ProtocolGlitchException(objectDisposedEx.Message, objectDisposedEx)
            | None ->
                ()
        | None ->
            ()

    let private ReworkException (ex: Exception): unit =
        let maybeWebEx = FSharpUtil.FindException<WebException> ex
        match maybeWebEx with
        | Some webEx ->

            // TODO: send a warning in Sentry
            if webEx.Status = WebExceptionStatus.UnknownError then
                raise <| ServerUnreachableException(exMsg, webEx)

            if webEx.Status = WebExceptionStatus.NameResolutionFailure then
                raise <| ServerCannotBeResolvedException(exMsg, webEx)
            if webEx.Status = WebExceptionStatus.ReceiveFailure then
                raise <| ServerTimedOutException(exMsg, webEx)
            if webEx.Status = WebExceptionStatus.ConnectFailure then
                raise <| ServerUnreachableException(exMsg, webEx)

            if webEx.Status = WebExceptionStatus.SecureChannelFailure then
                raise <| ServerChannelNegotiationException(exMsg, webEx.Status, webEx)
            if webEx.Status = WebExceptionStatus.RequestCanceled then
                raise <| ServerChannelNegotiationException(exMsg, webEx.Status, webEx)
            if (webEx.Status = WebExceptionStatus.TrustFailure) then
                raise <| ServerChannelNegotiationException(exMsg, webEx.Status, webEx)

            // as Ubuntu 18.04's Mono (4.6.2) doesn't have TLS1.2 support, this below is more likely to happen:
            if not Networking.Tls12Support then
                if webEx.Status = WebExceptionStatus.SendFailure then
                    raise <| ServerUnreachableException(exMsg, webEx)

            raise <| UnhandledWebException(webEx.Status, webEx)

        | None ->
            MaybeRethrowHttpRequestException ex

            MaybeRethrowRpcResponseException ex

            MaybeRethrowRpcClientTimeoutException ex

            MaybeRethrowNetworkingException ex

            MaybeRethrowObjectDisposedException ex


    let HandlePossibleEtherFailures<'T,'R> (job: Async<'R>): Async<'R> = async {
        try
            let! result = PerformEtherRemoteCallWithTimeout job
            return result
        with
        | ex ->
            ReworkException ex

            return raise <| FSharpUtil.ReRaise ex
    }

    let private NumberOfParallelJobsForMode mode =
        match mode with
        | Mode.Fast -> 5u
        | Mode.Analysis -> 3u

    let private FaultTolerantParallelClientInnerSettings (numberOfConsistentResponsesRequired: uint32)
                                                         (mode: Mode) =
        {
            NumberOfParallelJobsAllowed = NumberOfParallelJobsForMode mode
            ConsistencyConfig = SpecificNumberOfConsistentResponsesRequired numberOfConsistentResponsesRequired;
            NumberOfRetries = Config.NUMBER_OF_RETRIES_TO_SAME_SERVERS;
            NumberOfRetriesForInconsistency = Config.NUMBER_OF_RETRIES_TO_SAME_SERVERS;
            Mode = mode
            ShouldReportUncancelledJobs = true
        }

    let private FaultTolerantParallelClientDefaultSettings (currency: Currency) (mode: Mode) =
        let numberOfConsistentResponsesRequired =
            if not Networking.Tls12Support then
                1u
            else
                2u
        FaultTolerantParallelClientInnerSettings numberOfConsistentResponsesRequired
                                                 mode

    let private FaultTolerantParallelClientSettingsForBalanceCheck (currency: Currency)
                                                                   (mode: Mode)
                                                                   (cacheMatchFunc: decimal->bool) =
        let defaultSettings = FaultTolerantParallelClientDefaultSettings currency mode
        if mode = Mode.Fast then
            {
                defaultSettings with
                    ConsistencyConfig = OneServerConsistentWithCacheOrTwoServers cacheMatchFunc
            }
        else
            defaultSettings

    let private FaultTolerantParallelClientSettingsForBroadcast () =
        FaultTolerantParallelClientInnerSettings 1u Mode.Fast

    let private NUMBER_OF_CONSISTENT_RESPONSES_TO_TRUST_ETH_SERVER_RESULTS = 2
    let private NUMBER_OF_ALLOWED_PARALLEL_CLIENT_QUERY_JOBS = 3

    let private faultTolerantEtherClient =
        JsonRpcSharp.Client.RpcClient.ConnectionTimeout <- Config.DEFAULT_NETWORK_TIMEOUT
        FaultTolerantParallelClient<string,CommunicationUnsuccessfulException> Caching.Instance.SaveServerLastStat

    // FIXME: seems there's some code duplication between this function and UtxoAccount's GetRandomizedFuncs function
    let private GetWeb3Funcs<'T,'R> (currency: Currency)
                                    (web3Func: SomeWeb3->Async<'R>)
                                        : List<Server<string,'R>> =

        let Web3ServerToRetrievalFunc (web3Server: SomeWeb3)
                                          (web3ClientFunc: SomeWeb3->Async<'R>)
                                              : Async<'R> = async {
            try
                return! web3Func web3Server

            // NOTE: try to make this 'with' block be in sync with the one in UtxoCoinAccount:GetRandomizedFuncs()
            with
            | :? CommunicationUnsuccessfulException as ex ->
                return raise <| FSharpUtil.ReRaise ex
            | ex ->
                return raise <| Exception(sprintf "Some problem when connecting to %s" web3Server.Url, ex)
        }

        let Web3ServerToGenericServer (web3ClientFunc: SomeWeb3->Async<'R>)
                                      (web3Server: SomeWeb3)
                                              : Server<string,'R> =

            let retrievalFunc = Web3ServerToRetrievalFunc web3Server web3ClientFunc
            { Identifier = web3Server.Url
              HistoryInfo = Caching.Instance.RetreiveLastServerHistory web3Server.Url
              Retrieval = retrievalFunc }

        let web3servers = Web3ServerSeedList.Randomize currency |> List.ofSeq
        let serverFuncs =
            List.map (Web3ServerToGenericServer web3Func)
                     web3servers
        serverFuncs

    let GetTransactionCount (currency: Currency) (address: string)
                                : Async<HexBigInteger> =
        async {
            let web3Funcs =
                let web3Func (web3: Web3): Async<HexBigInteger> =
                    let transactionCountJob =
                        async {
                            let! cancelToken = Async.CancellationToken
                            let task =
                                web3.Eth.Transactions.GetTransactionCount.SendRequestAsync(address, null, cancelToken)
                            return! Async.AwaitTask task
                        }
                    HandlePossibleEtherFailures transactionCountJob
                GetWeb3Funcs currency web3Func
            return! faultTolerantEtherClient.Query
                (FaultTolerantParallelClientDefaultSettings currency Mode.Fast)
                web3Funcs
        }

    let private NUMBER_OF_CONFIRMATIONS_TO_CONSIDER_BALANCE_CONFIRMED = BigInteger(45)
    let private GetBlockToCheckForConfirmedBalance(web3: Web3): Async<BlockParameter> =
        async {
            let! cancelToken = Async.CancellationToken
            let! latestBlock =
                web3.Eth.Blocks.GetBlockNumber.SendRequestAsync (null, cancelToken)
                    |> Async.AwaitTask
            if (latestBlock = null) then
                failwith "latestBlock somehow is null"

            let blockToCheck = BigInteger.Subtract(latestBlock.Value,
                                                   NUMBER_OF_CONFIRMATIONS_TO_CONSIDER_BALANCE_CONFIRMED)

            if blockToCheck.Sign < 0 then
                let errMsg = sprintf
                                 "Looks like we received a wrong latestBlock(%s) because the substract was negative(%s)"
                                     (latestBlock.Value.ToString())
                                     (blockToCheck.ToString())
                raise <| ServerMisconfiguredException errMsg

            return BlockParameter(HexBigInteger(blockToCheck))
        }

    let private GetConfirmedEtherBalanceInternal (web3: Web3) (publicAddress: string): Async<HexBigInteger> =
        async {
            let! blockForConfirmationReference = GetBlockToCheckForConfirmedBalance web3
(*
            if (Config.DebugLog) then
                Console.Error.WriteLine (sprintf "Last block number and last confirmed block number: %s: %s"
                                                 (latestBlock.Value.ToString()) (blockForConfirmationReference.BlockNumber.Value.ToString()))
*)

            let! cancelToken = Async.CancellationToken
            cancelToken.ThrowIfCancellationRequested()
            let! balance =
                web3.Eth.GetBalance.SendRequestAsync (publicAddress,
                                                      blockForConfirmationReference,
                                                      null,
                                                      cancelToken)
                    |> Async.AwaitTask
            return balance
        }

    let private CachedBalanceMatch address currency someBalanceRetreived =
        match Caching.Instance.TryRetreiveLastCompoundBalance address currency with
        | None -> false
        | Some balance -> someBalanceRetreived = balance

    let GetEtherBalance (currency: Currency) (address: string) (balType: BalanceType) (mode: Mode)
                        (cancelSourceOption: Option<CancellationTokenSource>)
                                     : Async<decimal> =
        async {
            let web3Funcs =
                let web3Func (web3: Web3): Async<decimal> = async {
                    let job =
                        match balType with
                        | BalanceType.Confirmed ->
                            GetConfirmedEtherBalanceInternal web3 address
                        | BalanceType.Unconfirmed ->
                            async {
                                let! cancelToken = Async.CancellationToken
                                let task = web3.Eth.GetBalance.SendRequestAsync (address, null, cancelToken)
                                return! Async.AwaitTask task
                            }
                    let! balance = HandlePossibleEtherFailures job
                    return UnitConversion.Convert.FromWei(balance.Value, UnitConversion.EthUnit.Ether)
                }
                GetWeb3Funcs currency web3Func

            let query =
                match cancelSourceOption with
                | None ->
                    faultTolerantEtherClient.Query
                | Some cancelSource ->
                    faultTolerantEtherClient.QueryWithCancellation cancelSource

            return! query
                        (FaultTolerantParallelClientSettingsForBalanceCheck
                            currency mode (CachedBalanceMatch address currency))
                        web3Funcs
        }

    let private GetConfirmedTokenBalanceInternal (web3: Web3) (publicAddress: string): Async<decimal> =
        if (web3 = null) then
            invalidArg "web3" "web3 argument should not be null"

        async {
            let! blockForConfirmationReference = GetBlockToCheckForConfirmedBalance web3
            let balanceOfFunctionMsg = BalanceOfFunction(Owner = publicAddress)

            let contractHandler = web3.Eth.GetContractHandler(TokenManager.DAI_CONTRACT_ADDRESS)
            if (contractHandler = null) then
                failwith "contractHandler somehow is null"

            let! cancelToken = Async.CancellationToken
            cancelToken.ThrowIfCancellationRequested()
            let! balance = contractHandler.QueryAsync<BalanceOfFunction,BigInteger>
                                    (balanceOfFunctionMsg,
                                     blockForConfirmationReference,
                                     cancelToken) |> Async.AwaitTask
            return UnitConversion.Convert.FromWei(balance, UnitConversion.EthUnit.Ether)
        }


    let GetTokenBalance (currency: Currency)
                        (address: string)
                        (balType: BalanceType)
                        (mode: Mode)
                        (cancelSourceOption: Option<CancellationTokenSource>)
                            : Async<decimal> =
        async {
            let web3Funcs =
                let web3Func (web3: Web3): Async<decimal> =
                    let job =
                        match balType with
                        | BalanceType.Confirmed ->
                            GetConfirmedTokenBalanceInternal web3 address
                        | BalanceType.Unconfirmed ->
                            let tokenService = TokenManager.DaiContract web3
                            async {
                                let! cancelToken = Async.CancellationToken
                                let task = tokenService.BalanceOfQueryAsync (address, null, cancelToken)
                                let! balance = Async.AwaitTask task
                                return UnitConversion.Convert.FromWei(balance, UnitConversion.EthUnit.Ether)
                            }

                    HandlePossibleEtherFailures job
                GetWeb3Funcs currency web3Func

            let query =
                match cancelSourceOption with
                | None ->
                    faultTolerantEtherClient.Query
                | Some cancelSource ->
                    faultTolerantEtherClient.QueryWithCancellation cancelSource

            return! query
                        (FaultTolerantParallelClientSettingsForBalanceCheck
                            currency mode (CachedBalanceMatch address currency))
                        web3Funcs
        }

    let EstimateTokenTransferFee (baseCurrency: Currency) (account: IAccount) (amount: decimal) destination
                                     : Async<HexBigInteger> =
        async {
            let web3Funcs =
                let web3Func (web3: Web3): Async<HexBigInteger> =
                    let contractHandler = web3.Eth.GetContractHandler(TokenManager.DAI_CONTRACT_ADDRESS)
                    let amountInWei = UnitConversion.Convert.ToWei(amount, UnitConversion.EthUnit.Ether)
                    let transferFunctionMsg = TransferFunction(FromAddress = account.PublicAddress,
                                                               To = destination,
                                                               Value = amountInWei)
                    let gasJob =
                        async {
                            let! cancelToken = Async.CancellationToken
                            let task =
                                contractHandler.EstimateGasAsync<TransferFunction>(transferFunctionMsg, cancelToken)
                            return! Async.AwaitTask task
                        }
                    HandlePossibleEtherFailures gasJob
                GetWeb3Funcs account.Currency web3Func
            return! faultTolerantEtherClient.Query
                        (FaultTolerantParallelClientDefaultSettings baseCurrency Mode.Fast)
                        web3Funcs
        }

    let private AverageGasPrice (gasPricesFromDifferentServers: List<HexBigInteger>): HexBigInteger =
        let sum = gasPricesFromDifferentServers.Select(fun hbi -> hbi.Value)
                                               .Aggregate(fun bi1 bi2 -> BigInteger.Add(bi1, bi2))
        let avg = BigInteger.Divide(sum, BigInteger(gasPricesFromDifferentServers.Length))
        HexBigInteger(avg)

    let GetGasPrice (currency: Currency)
        : Async<HexBigInteger> =
        async {
            let web3Funcs =
                let web3Func (web3: Web3): Async<HexBigInteger> =
                    let gasPriceJob =
                        async {
                            let! cancelToken = Async.CancellationToken
                            let task = web3.Eth.GasPrice.SendRequestAsync(null, cancelToken)
                            return! Async.AwaitTask task
                        }
                    HandlePossibleEtherFailures gasPriceJob
                GetWeb3Funcs currency web3Func
            let minResponsesRequired = 2u
            return! faultTolerantEtherClient.Query
                        { FaultTolerantParallelClientDefaultSettings currency Mode.Fast with
                              ConsistencyConfig = AverageBetweenResponses (minResponsesRequired, AverageGasPrice) }
                        web3Funcs

        }

    let BroadcastTransaction (currency: Currency) (transaction: string)
        : Async<string> =
        let insufficientFundsMsg = "Insufficient funds"

        async {
            let web3Funcs =
                let web3Func (web3: Web3): Async<string> =
                    let broadcastJob =
                        async {
                            let! cancelToken = Async.CancellationToken
                            let task =
                                web3.Eth.Transactions.SendRawTransaction.SendRequestAsync(transaction, null, cancelToken)
                            return! Async.AwaitTask task
                        }
                    HandlePossibleEtherFailures broadcastJob
                GetWeb3Funcs currency web3Func
            try
                return! faultTolerantEtherClient.Query
                            (FaultTolerantParallelClientSettingsForBroadcast ())
                            web3Funcs
            with
            | ex ->
                match FSharpUtil.FindException<JsonRpcSharp.Client.RpcResponseException> ex with
                | None ->
                    return raise (FSharpUtil.ReRaise ex)
                | Some rpcResponseException ->
                    // FIXME: this is fragile, ideally should respond with an error code
                    if rpcResponseException.Message.StartsWith(insufficientFundsMsg,
                                                               StringComparison.InvariantCultureIgnoreCase) then
                        return raise InsufficientFunds
                    else
                        return raise (FSharpUtil.ReRaise ex)
        }

    let private GetTransactionDetailsFromTransactionReceipt (currency: Currency) (txHash: string)
                                          : Async<TransactionStatusDetails> =
        async {
            let web3Funcs =
                let web3Func (web3: Web3): Async<TransactionStatusDetails> =
                    async {
                        let! cancelToken = Async.CancellationToken
                        let task =
                            web3.TransactionManager.TransactionReceiptService.PollForReceiptAsync(txHash, cancelToken)
                        let! transactionReceipt = Async.AwaitTask task
                        return {
                            GasUsed = transactionReceipt.GasUsed.Value
                            Status = transactionReceipt.Status.Value
                        }
                    } |> HandlePossibleEtherFailures
                GetWeb3Funcs currency web3Func
            return! faultTolerantEtherClient.Query
                (FaultTolerantParallelClientDefaultSettings currency Mode.Fast)
                web3Funcs
        }

    let IsOutOfGas (currency: Currency) (txHash: string) (spentGas: int64): Async<bool> =
        async {
            let! transactionStatusDetails = GetTransactionDetailsFromTransactionReceipt currency txHash
            let failureStatus = BigInteger.Zero
            return transactionStatusDetails.Status = failureStatus &&
                   transactionStatusDetails.GasUsed = BigInteger(spentGas)
        }

    let private GetContractCode (baseCurrency: Currency) (address: string)
                                    : Async<string> =
        async {
            let web3Funcs =
                let web3Func (web3: Web3): Async<string> =
                    let contractCodeJob =
                        async {
                            let! cancelToken = Async.CancellationToken
                            let task = web3.Eth.GetCode.SendRequestAsync(address, null, cancelToken)
                            return! Async.AwaitTask task
                        }
                    HandlePossibleEtherFailures contractCodeJob
                GetWeb3Funcs baseCurrency web3Func
            return! faultTolerantEtherClient.Query
                (FaultTolerantParallelClientDefaultSettings baseCurrency Mode.Fast)
                web3Funcs
        }

    let CheckIfAddressIsAValidPaymentDestination (currency: Currency) (address: string): Async<unit> =
        async {
            let! contractCode = GetContractCode currency address
            let emptyContract = "0x"

            if not (contractCode.StartsWith emptyContract) then
                failwithf "GetCode API should always return a string starting with %s, but got: %s"
                          emptyContract contractCode
            elif contractCode <> emptyContract then
                return raise <| InvalidDestinationAddress "Sending to contract addresses is not supported yet. Supply a normal address please."
        }

