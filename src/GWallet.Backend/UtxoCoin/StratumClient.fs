﻿namespace GWallet.Backend.UtxoCoin

open System

open Newtonsoft.Json

open GWallet.Backend

// can't make this type below private, or else Newtonsoft.Json will serialize it incorrectly
type Request =
    {
        Id: int;
        Method: string;
        Params: seq<obj>;
    }

type ServerVersionResult =
    {
        Id: int;
        Result: array<string>;
    }

type BlockchainScripthahsGetBalanceInnerResult =
    {
        Confirmed: Int64;
        Unconfirmed: Int64;
    }
type BlockchainScripthashGetBalanceResult =
    {
        Id: int;
        Result: BlockchainScripthahsGetBalanceInnerResult;
    }

type BlockchainScripthashListUnspentInnerResult =
    {
        TxHash: string;
        TxPos: int;
        Value: Int64;
        Height: Int64;
    }
type BlockchainScripthashListUnspentResult =
    {
        Id: int;
        Result: array<BlockchainScripthashListUnspentInnerResult>;
    }

type BlockchainTransactionGetResult =
    {
        Id: int;
        Result: string;
    }

type BlockchainEstimateFeeResult =
    {
        Id: int;
        Result: decimal;
    }

type BlockchainTransactionBroadcastResult =
    {
        Id: int;
        Result: string;
    }

type ErrorInnerResult =
    {
        Message: string;
        Code: int;
    }

type ErrorResult =
    {
        Id: int;
        Error: ErrorInnerResult;
    }

type RpcErrorCode =
    // see https://gitlab.com/knocte/geewallet/issues/110
    | ExcessiveResourceUsage = -101

    // see https://gitlab.com/knocte/geewallet/issues/117
    | ServerBusy = -102

    // see git commit msg of 0aba03a8291daa526fde888d0c02a789abe411f2
    | InternalError = -32603

    // see https://gitlab.com/knocte/geewallet/issues/112
    | UnknownMethod = -32601

type public ElectrumServerReturningErrorInJsonResponseException(message: string, code: int) =
    inherit CommunicationUnsuccessfulException(message)

    member val ErrorCode: int =
        code with get

type public ElectrumServerReturningErrorException(message: string, code: int,
                                                  originalRequest: string, originalResponse: string) =
    inherit ElectrumServerReturningErrorInJsonResponseException(message, code)

    member val OriginalRequest: string =
        originalRequest with get

    member val OriginalResponse: string =
        originalResponse with get

type public ElectrumServerReturningInternalErrorException(message: string, code: int,
                                                          originalRequest: string, originalResponse: string) =
    inherit ElectrumServerReturningErrorException(message, code, originalRequest, originalResponse)

// FIXME: we should actually fix this bug in JsonRpcSharp (https://github.com/nblockchain/JsonRpcSharp/issues/9) and
//        send a warning to Sentry, not just hide it under the rug
type FlakyJsonRpcSharpClientException (message: string) =
    inherit CommunicationUnsuccessfulException(message)

type StratumClient (jsonRpcClient: JsonRpcTcpClient) =

    let Serialize(req: Request): string =
        JsonConvert.SerializeObject(req, Formatting.None,
                                    Marshalling.PascalCase2LowercasePlusUnderscoreConversionSettings)

    // TODO: add 'T as incoming request type, leave 'R as outgoing response type
    member private self.Request<'R> (jsonRequest: string): Async<'R> = async {
        let! rawResponse = jsonRpcClient.Request jsonRequest
        if String.IsNullOrEmpty rawResponse then
            return raise <|
                FlakyJsonRpcSharpClientException(
                    sprintf "Server '%s' returned a null/empty JSON response to the request '%s'"
                            jsonRpcClient.Host jsonRequest)
        try
            return StratumClient.Deserialize<'R> rawResponse
        with
        | :? ElectrumServerReturningErrorInJsonResponseException as ex ->
            if ex.ErrorCode = int RpcErrorCode.InternalError then
                return raise(ElectrumServerReturningInternalErrorException(ex.Message, ex.ErrorCode, jsonRequest, rawResponse))
            if ex.ErrorCode = int RpcErrorCode.UnknownMethod then
                return raise <| ServerMisconfiguredException(ex.Message, ex)
            if ex.ErrorCode = int RpcErrorCode.ServerBusy then
                return raise <| ServerUnavailabilityException(ex.Message, ex)
            if ex.ErrorCode = int RpcErrorCode.ExcessiveResourceUsage then
                return raise <| ServerUnavailabilityException(ex.Message, ex)

            return raise(ElectrumServerReturningErrorException(ex.Message, ex.ErrorCode, jsonRequest, rawResponse))
    }

    static member public Deserialize<'T> (result: string): 'T =
        let resultTrimmed = result.Trim()
        let maybeError =
            try
                JsonConvert.DeserializeObject<ErrorResult>(resultTrimmed,
                                                           Marshalling.PascalCase2LowercasePlusUnderscoreConversionSettings)
            with
            | ex -> raise <| Exception(sprintf "Failed deserializing JSON response (to check for error) '%s' to type '%s'"
                                               resultTrimmed typedefof<'T>.FullName, ex)

        if (not (Object.ReferenceEquals(maybeError, null))) && (not (Object.ReferenceEquals(maybeError.Error, null))) then
            raise(ElectrumServerReturningErrorInJsonResponseException(maybeError.Error.Message, maybeError.Error.Code))

        let deserializedValue =
            try
                JsonConvert.DeserializeObject<'T>(resultTrimmed,
                                                  Marshalling.PascalCase2LowercasePlusUnderscoreConversionSettings)
            with
            | ex -> raise <| Exception(sprintf "Failed deserializing JSON response '%s' to type '%s'"
                                                resultTrimmed typedefof<'T>.FullName, ex)

        if Object.ReferenceEquals(deserializedValue, null) then
            failwithf "Failed deserializing JSON response '%s' to type '%s' (result was null)"
                      resultTrimmed typedefof<'T>.FullName

        deserializedValue

    member self.BlockchainScripthashGetBalance address: Async<BlockchainScripthashGetBalanceResult> =
        let obj = {
            Id = 0;
            Method = "blockchain.scripthash.get_balance";
            Params = [address]
        }
        let json = Serialize obj

        self.Request<BlockchainScripthashGetBalanceResult> json

    static member private CreateVersion(versionStr: string): Version =
        let correctedVersion =
            if (versionStr.EndsWith("+")) then
                versionStr.Substring(0, versionStr.Length - 1)
            else
                versionStr
        try
            Version(correctedVersion)
        with
        | exn -> raise(Exception("Electrum Server's version disliked by .NET Version class: " + versionStr, exn))

    member self.ServerVersion (clientName: string) (protocolVersion: Version): Async<Version> = async {
        let obj = {
            Id = 0;
            Method = "server.version";
            Params = [clientName; protocolVersion.ToString()]
        }
        // this below serializes to:
        //  (sprintf "{ \"id\": 0, \"method\": \"server.version\", \"params\": [ \"%s\", \"%s\" ] }"
        //      CURRENT_ELECTRUM_FAKED_VERSION PROTOCOL_VERSION)
        let json = Serialize obj
        let! resObj = self.Request<ServerVersionResult> json

        // e.g. "ElectrumX 1.4.3"
        let serverNameAndVersion = resObj.Result.[0]
        // e.g. "1.1"
        let serverProtocolVersion = resObj.Result.[1]

        return StratumClient.CreateVersion(serverProtocolVersion)
    }

    member self.BlockchainScripthashListUnspent address: Async<BlockchainScripthashListUnspentResult> =
        let obj = {
            Id = 0;
            Method = "blockchain.scripthash.listunspent";
            Params = [address]
        }
        let json = Serialize obj
        let resObj = self.Request<BlockchainScripthashListUnspentResult> json
        resObj

    member self.BlockchainTransactionGet txHash: Async<BlockchainTransactionGetResult> =
        let obj = {
            Id = 0;
            Method = "blockchain.transaction.get";
            Params = [txHash]
        }
        let json = Serialize obj

        self.Request<BlockchainTransactionGetResult> json

    member self.BlockchainEstimateFee (numBlocksTarget: int): Async<BlockchainEstimateFeeResult> =
        let obj = {
            Id = 0;
            Method = "blockchain.estimatefee";
            Params = [numBlocksTarget]
        }
        let json = Serialize obj

        self.Request<BlockchainEstimateFeeResult> json

    member self.BlockchainTransactionBroadcast txInHex: Async<BlockchainTransactionBroadcastResult> =
        let obj = {
            Id = 0;
            Method = "blockchain.transaction.broadcast";
            Params = [txInHex]
        }
        let json = Serialize obj

        self.Request<BlockchainTransactionBroadcastResult> json
