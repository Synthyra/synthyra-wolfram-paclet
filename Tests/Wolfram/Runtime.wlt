Needs["Synthyra`SynthyraLink`"];

ClearAll[mockResponse];
mockResponse[status_Integer, body_String : "", contentType_String : "application/json", headers_List : {}] := <|
    "StatusCode" -> status,
    "Headers" -> headers,
    "ContentType" -> contentType,
    "Body" -> body,
    "BodyByteArray" -> StringToByteArray[body, "UTF8"]
|>;

ClearAll[writeTestBytes, readTestBytes, createTestDownloadDirectory];
writeTestBytes[path_String, bytes_List] := Module[{stream},
    stream = OpenWrite[path, BinaryFormat -> True];
    BinaryWrite[stream, bytes, "UnsignedInteger8"];
    Close[stream];
    File[path]
];
readTestBytes[path_String] := Module[{stream, bytes},
    stream = OpenRead[path, BinaryFormat -> True];
    bytes = BinaryReadList[stream, "UnsignedInteger8"];
    Close[stream];
    bytes
];
createTestDownloadDirectory[] := CreateDirectory[
    FileNameJoin[{$TemporaryDirectory, "synthyralink-test-" <> CreateUUID[]}]
];

$testValidators = <|
    "SubmitBody" -> <|
        "Kind" -> "Object",
        "Nullable" -> False,
        "AdditionalProperties" -> False,
        "Properties" -> <|
            "proteinIds" -> <|
                "Required" -> True,
                "Schema" -> <|
                    "Kind" -> "Array",
                    "Nullable" -> False,
                    "Constraints" -> <|"minItems" -> 2|>,
                    "Items" -> <|"Kind" -> "String", "Nullable" -> False|>
                |>
            |>
        |>
    |>
|>;

$testOperations = <|
    "GetHealth" -> <|
        "Authentication" -> <|"Anonymous" -> True, "Schemes" -> {}|>,
        "Method" -> "GET", "Path" -> "/health", "Parameters" -> {},
        "RequestBody" -> Null, "ResultKind" -> "JSON"
    |>,
    "GetOptional" -> <|
        "Authentication" -> <|"Anonymous" -> True, "Schemes" -> {"BearerAuth"}|>,
        "Method" -> "GET", "Path" -> "/optional", "Parameters" -> {},
        "RequestBody" -> Null, "ResultKind" -> "JSON"
    |>,
    "GetExample" -> <|
        "Authentication" -> <|"Anonymous" -> False, "Schemes" -> {"BearerAuth"}|>,
        "Method" -> "GET", "Path" -> "/example", "Parameters" -> {},
        "RequestBody" -> Null, "ResultKind" -> "JSON"
    |>,
    "StreamExample" -> <|
        "Authentication" -> <|"Anonymous" -> False, "Schemes" -> {"BearerAuth"}|>,
        "Method" -> "GET", "Path" -> "/stream", "Parameters" -> {},
        "RequestBody" -> Null, "ResultKind" -> "JSONL"
    |>,
    "DownloadExample" -> <|
        "Authentication" -> <|"Anonymous" -> False, "Schemes" -> {"BearerAuth"}|>,
        "Method" -> "GET", "Path" -> "/download", "Parameters" -> {},
        "RequestBody" -> Null, "ResultKind" -> "Binary"
    |>,
    "SubmitExample" -> <|
        "Authentication" -> <|"Anonymous" -> False, "Schemes" -> {"BearerAuth"}|>,
        "Method" -> "POST", "Path" -> "/jobs", "Parameters" -> {},
        "RequestBody" -> <|"Required" -> True, "Schema" -> <|"Kind" -> "Reference", "Reference" -> "SubmitBody", "Nullable" -> False|>|>,
        "ResultKind" -> "Job",
        "Async" -> <|
            "StatusOperationId" -> "getStatus", "ResultOperationId" -> "getResult",
            "CancelOperationId" -> "cancelJob", "IdentifierParameter" -> "jobId",
            "TerminalStates" -> {"succeeded", "failed", "cancelled"},
            "SuccessStates" -> {"succeeded"}, "FailureStates" -> {"failed", "cancelled"}
        |>
    |>,
    "GetStatus" -> <|
        "Authentication" -> <|"Anonymous" -> False, "Schemes" -> {"BearerAuth"}|>,
        "Method" -> "GET", "Path" -> "/jobs/{jobId}",
        "Parameters" -> {<|"Name" -> "jobId", "In" -> "Path", "Required" -> True, "Style" -> "simple", "Explode" -> False, "Schema" -> <|"Kind" -> "String", "Nullable" -> False|>|>},
        "RequestBody" -> Null, "ResultKind" -> "JSON"
    |>,
    "GetResult" -> <|
        "Authentication" -> <|"Anonymous" -> False, "Schemes" -> {"BearerAuth"}|>,
        "Method" -> "GET", "Path" -> "/jobs/{jobId}/result",
        "Parameters" -> {<|"Name" -> "jobId", "In" -> "Path", "Required" -> True, "Style" -> "simple", "Explode" -> False, "Schema" -> <|"Kind" -> "String", "Nullable" -> False|>|>},
        "RequestBody" -> Null, "ResultKind" -> "JSON"
    |>,
    "CancelJob" -> <|
        "Authentication" -> <|"Anonymous" -> False, "Schemes" -> {"BearerAuth"}|>,
        "Method" -> "POST", "Path" -> "/jobs/{jobId}/cancel",
        "Parameters" -> {<|"Name" -> "jobId", "In" -> "Path", "Required" -> True, "Style" -> "simple", "Explode" -> False, "Schema" -> <|"Kind" -> "String", "Nullable" -> False|>|>},
        "RequestBody" -> Null, "ResultKind" -> "JSON"
    |>
|>;

$testManifest = <|"getStatus" -> "GetStatus", "getResult" -> "GetResult", "cancelJob" -> "CancelJob"|>;

VerificationTest[
    Module[{secret = "unit-test-secret", client},
        client = SynthyraConnect[Authentication -> secret];
        FreeQ[client, secret] && FreeQ[Normal[client], secret]
    ],
    True,
    TestID -> "credential-never-stored-in-client-expression@@Tests/Wolfram/Runtime.wlt:108,1-115,2"
];

VerificationTest[
    Module[{client, seen = None, result},
        client = SynthyraConnect[Authentication -> None];
        result = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout}, seen = request; mockResponse[200, "{\"status\":\"ok\"}"]]
            },
            client["GetHealth"]
        ];
        result === <|"status" -> "ok"|> && ! KeyExistsQ[seen[[2]], "Body"] &&
            FreeQ[seen[[2, "Headers"]], "Authorization"]
    ],
    True,
    TestID -> "anonymous-get-has-no-auth-or-body@@Tests/Wolfram/Runtime.wlt:117,1-132,2"
];

VerificationTest[
    Module[{client, seen = None},
        client = SynthyraConnect[Authentication -> "configured-key"];
        Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout}, seen = request; mockResponse[200, "{}"]]
            },
            client["GetOptional"]
        ];
        MemberQ[seen[[2, "Headers"]], "Authorization" -> "Bearer configured-key"]
    ],
    True,
    TestID -> "optional-bearer-uses-configured-key@@Tests/Wolfram/Runtime.wlt:134,1-148,2"
];

VerificationTest[
    Module[{client = SynthyraConnect[Authentication -> None]},
        Block[{Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations},
            FailureQ[client["GetExample"]]
        ]
    ],
    True,
    TestID -> "required-bearer-fails-closed@@Tests/Wolfram/Runtime.wlt:150,1-158,2"
];

VerificationTest[
    {
        Synthyra`SynthyraLink`Private`serializeQueryParameter[
            <|"Style" -> "form", "Explode" -> True|>, "proteinIds", {"P1", "P2"}
        ],
        Synthyra`SynthyraLink`Private`serializeQueryParameter[
            <|"Style" -> "form", "Explode" -> False|>, "proteinIds", {"P1", "P2"}
        ]
    },
    {{"proteinIds" -> "P1", "proteinIds" -> "P2"}, {"proteinIds" -> "P1,P2"}},
    TestID -> "openapi-form-array-explode-semantics@@Tests/Wolfram/Runtime.wlt:160,1-171,2"
];

VerificationTest[
    Module[{client, calls = 0, result},
        client = SynthyraConnect[Authentication -> "secret"];
        result = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout}, calls++; mockResponse[200, "{}"]]
            },
            client["SubmitExample", <|"proteinIds" -> {"only-one"}|>]
        ];
        FailureQ[result] && calls === 0 && FreeQ[result, "only-one"]
    ],
    True,
    TestID -> "generated-request-validation-before-http@@Tests/Wolfram/Runtime.wlt:173,1-188,2"
];

VerificationTest[
    Module[{client, calls = 0, failure},
        client = SynthyraConnect[Authentication -> "secret"];
        failure = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout}, calls++; mockResponse[200, "{}"]]
            },
            client[
                "GetStatus",
                <|"jobId" -> "job-1", "jobld" -> "sensitive-parameter-value"|>
            ]
        ];
        FailureQ[failure] && calls === 0 &&
            failure[[1]] === "RequestValidationFailed" &&
            Lookup[failure[[2]], "Operation"] === "GetStatus" &&
            Lookup[failure[[2]], "Field"] === "Parameters.jobld" &&
            FreeQ[failure, "sensitive-parameter-value"]
    ],
    True,
    TestID -> "undeclared-parameter-typo-fails-before-http-without-value-leak@@Tests/Wolfram/Runtime.wlt:190,1-211,2"
];

VerificationTest[
    Module[{client, calls = 0, result},
        client = SynthyraConnect[Authentication -> "secret"];
        result = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout},
                    calls++;
                    mockResponse[200, "{\"job_id\":\"job-1\",\"status\":\"queued\"}"]
                ]
            },
            client[
                "SubmitExample",
                <|
                    "Parameters" -> <||>,
                    "Body" -> <|"proteinIds" -> {"P1", "P2"}|>
                |>
            ]
        ];
        Head[result] === SynthyraJobObject && calls === 1
    ],
    True,
    TestID -> "reserved-parameters-and-body-wrapper-keys-remain-supported@@Tests/Wolfram/Runtime.wlt:213,1-237,2"
];

VerificationTest[
    Module[{client, seen = None, result, requestData, parsedBody},
        client = SynthyraConnect[Authentication -> "secret"];
        result = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout},
                    seen = request;
                    mockResponse[200, "{\"job_id\":\"job-1\",\"status\":\"queued\"}"]
                ]
            },
            client["SubmitExample", <|"proteinIds" -> {"P1", "P2"}|>, MaxRetries -> 0]
        ];
        requestData = seen[[2]];
        parsedBody = ImportString[Lookup[requestData, "Body", ""], "RawJSON"];
        Head[result] === SynthyraJobObject &&
            Lookup[requestData, "ContentType", Missing["NotAvailable"]] === "application/json" &&
            parsedBody === <|"proteinIds" -> {"P1", "P2"}|>
    ],
    True,
    TestID -> "post-request-includes-json-body-and-content-type@@Tests/Wolfram/Runtime.wlt:239,1-261,2"
];

VerificationTest[
    Module[{client, calls = 0},
        client = SynthyraConnect[Authentication -> "secret"];
        Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout},
                    calls++;
                    If[calls < 3, mockResponse[503, "{}"], mockResponse[200, "{\"ok\":true}"]]
                ],
                Synthyra`SynthyraLink`Private`$SynthyraPauseFunction = Function[Null]
            },
            client["GetExample", MaxRetries -> 5]
        ];
        calls
    ],
    3,
    TestID -> "safe-get-retries-transient-status@@Tests/Wolfram/Runtime.wlt:263,1-281,2"
];

VerificationTest[
    Module[{client, calls = 0, result},
        client = SynthyraConnect[Authentication -> "secret"];
        result = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout}, calls++; mockResponse[503, "{}"]]
            },
            client["SubmitExample", <|"proteinIds" -> {"P1", "P2"}|>, MaxRetries -> 10]
        ];
        FailureQ[result] && calls === 1
    ],
    True,
    TestID -> "post-submission-never-retries@@Tests/Wolfram/Runtime.wlt:283,1-298,2"
];

VerificationTest[
    Module[{client, result},
        client = SynthyraConnect[Authentication -> "secret"];
        result = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout},
                    mockResponse[200, "{\"id\":1}\n{\"id\":2}\n", "application/x-ndjson"]
                ]
            },
            client["StreamExample"]
        ];
        result
    ],
    {<|"id" -> 1|>, <|"id" -> 2|>},
    TestID -> "jsonl-parsing@@Tests/Wolfram/Runtime.wlt:300,1-316,2"
];

VerificationTest[
    Module[{client, raw},
        client = SynthyraConnect[Authentication -> "raw-secret"];
        raw = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout},
                    mockResponse[
                        400,
                        "{\"token\":\"raw-secret\",\"detail\":\"Bearer raw-secret\"}",
                        "application/json",
                        {"Authorization" -> "Bearer raw-secret", "Set-Cookie" -> "token=raw-secret", "X-Request-ID" -> "request-1"}
                    ]
                ]
            },
            client["GetExample", RawResponse -> True]
        ];
        Sort[Keys[raw]] === Sort[{"StatusCode", "Headers", "Body"}] && FreeQ[raw, "raw-secret"] &&
            FreeQ[Keys[raw["Headers"]], "Authorization" | "Set-Cookie"]
    ],
    True,
    TestID -> "raw-response-is-minimal-and-redacted@@Tests/Wolfram/Runtime.wlt:318,1-340,2"
];

VerificationTest[
    Module[{client, calls = 0, seen = None, raw},
        client = SynthyraConnect[Authentication -> "secret"];
        raw = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout},
                    calls++;
                    seen = request;
                    mockResponse[418, "{\"detail\":\"fixture\"}"]
                ]
            },
            client["GetExample", RawResponse -> True]
        ];
        {AssociationQ[raw], Lookup[raw, "StatusCode", Missing["NotAvailable"]], calls, ! KeyExistsQ[seen[[2]], "Body"]}
    ],
    {True, 418, 1, True},
    TestID -> "client-object-options-only-dispatch@@Tests/Wolfram/Runtime.wlt:342,1-360,2"
];

VerificationTest[
    Module[{client, calls = 0, seen = None, raw},
        client = SynthyraConnect[Authentication -> "secret"];
        raw = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout},
                    calls++;
                    seen = request;
                    mockResponse[409, "{\"detail\":\"fixture\"}"]
                ]
            },
            SynthyraExecute[client, "GetExample", RawResponse -> True]
        ];
        {AssociationQ[raw], Lookup[raw, "StatusCode", Missing["NotAvailable"]], calls, ! KeyExistsQ[seen[[2]], "Body"]}
    ],
    {True, 409, 1, True},
    TestID -> "functional-options-only-dispatch@@Tests/Wolfram/Runtime.wlt:362,1-380,2"
];

VerificationTest[
    Module[{lowerSchema, upperSchema, lowerBoundary, upperBoundary},
        lowerSchema = <|
            "Kind" -> "Integer", "Nullable" -> False,
            "Constraints" -> <|"exclusiveMinimum" -> 0.0|>
        |>;
        upperSchema = <|
            "Kind" -> "Number", "Nullable" -> False,
            "Constraints" -> <|"exclusiveMaximum" -> 1.0|>
        |>;
        lowerBoundary = Synthyra`SynthyraLink`Private`validateSchemaValue[0, lowerSchema, "Bounds", "Body.lower"];
        upperBoundary = Synthyra`SynthyraLink`Private`validateSchemaValue[1.0, upperSchema, "Bounds", "Body.upper"];
        {
            FailureQ[lowerBoundary], Lookup[lowerBoundary[[2]], "Reason"],
            TrueQ @ Synthyra`SynthyraLink`Private`validateSchemaValue[1, lowerSchema, "Bounds", "Body.lower"],
            FailureQ[upperBoundary], Lookup[upperBoundary[[2]], "Reason"],
            TrueQ @ Synthyra`SynthyraLink`Private`validateSchemaValue[0.5, upperSchema, "Bounds", "Body.upper"]
        }
    ],
    {True, "not above exclusiveMinimum", True, True, "not below exclusiveMaximum", True},
    TestID -> "exclusive-numeric-bounds-reject-equality@@Tests/Wolfram/Runtime.wlt:382,1-403,2"
];

VerificationTest[
    Module[{schema, valid, invalid},
        schema = <|
            "Kind" -> "Object", "Nullable" -> False,
            "Properties" -> <|
                "known" -> <|
                    "Required" -> True,
                    "Schema" -> <|"Kind" -> "String", "Nullable" -> False|>
                |>
            |>,
            "AdditionalProperties" -> <|"Kind" -> "Integer", "Nullable" -> False|>
        |>;
        valid = Synthyra`SynthyraLink`Private`validateSchemaValue[
            <|"known" -> "fixture", "extra" -> 2|>, schema, "Additional", "Body"
        ];
        invalid = Synthyra`SynthyraLink`Private`validateSchemaValue[
            <|"known" -> "fixture", "extra" -> "two"|>, schema, "Additional", "Body"
        ];
        {
            TrueQ[valid], FailureQ[invalid], Lookup[invalid[[2]], "Field"], Lookup[invalid[[2]], "Reason"]
        }
    ],
    {True, True, "Body.extra", "expected integer"},
    TestID -> "schema-valued-additional-properties-validate-recursively@@Tests/Wolfram/Runtime.wlt:405,1-429,2"
];

VerificationTest[
    Module[{multipleSchema, uniqueSchema, objectSchema, results},
        multipleSchema = <|
            "Kind" -> "Number", "Nullable" -> False,
            "Constraints" -> <|"multipleOf" -> 0.25|>
        |>;
        uniqueSchema = <|
            "Kind" -> "Array", "Nullable" -> False,
            "Items" -> <|"Kind" -> "String", "Nullable" -> False|>,
            "Constraints" -> <|"uniqueItems" -> True|>
        |>;
        objectSchema = <|
            "Kind" -> "Object", "Nullable" -> False,
            "Properties" -> <||>, "AdditionalProperties" -> True,
            "Constraints" -> <|"minProperties" -> 1, "maxProperties" -> 2|>
        |>;
        results = {
            Synthyra`SynthyraLink`Private`validateSchemaValue[0.3, multipleSchema, "Constraints", "Body.multiple"],
            Synthyra`SynthyraLink`Private`validateSchemaValue[{"A", "A"}, uniqueSchema, "Constraints", "Body.unique"],
            Synthyra`SynthyraLink`Private`validateSchemaValue[<||>, objectSchema, "Constraints", "Body.object"],
            Synthyra`SynthyraLink`Private`validateSchemaValue[<|"a" -> 1, "b" -> 2, "c" -> 3|>, objectSchema, "Constraints", "Body.object"]
        };
        FailureQ /@ results
    ],
    {True, True, True, True},
    TestID -> "remaining-supported-json-schema-constraints-validate@@Tests/Wolfram/Runtime.wlt:431,1-457,2"
];

VerificationTest[
    Module[{client, failure},
        client = SynthyraConnect[Authentication -> "failure-secret"];
        failure = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout},
                    mockResponse[500, "{\"detail\":\"Bearer failure-secret\"}", "application/json", {"X-Synthyra-Request-ID" -> "request-2"}]
                ]
            },
            client["GetExample"]
        ];
        FailureQ[failure] && FreeQ[failure, "failure-secret"] && ! FreeQ[failure, "request-2"]
    ],
    True,
    TestID -> "http-failure-redacts-key-and-keeps-request-id@@Tests/Wolfram/Runtime.wlt:459,1-475,2"
];

VerificationTest[
    Module[{client, result},
        client = SynthyraConnect[Authentication -> "secret"];
        result = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout},
                    <|"StatusCode" -> 200, "Headers" -> {}, "ContentType" -> "application/octet-stream", "Body" -> "", "BodyByteArray" -> ByteArray[{0, 127, 255}]|>
                ]
            },
            client["DownloadExample"]
        ];
        Normal[result]
    ],
    {0, 127, 255},
    TestID -> "binary-result-is-byte-array@@Tests/Wolfram/Runtime.wlt:477,1-493,2"
];

VerificationTest[
    Module[{client, httpCalls = 0, downloadCalls = 0, directory, path, result, observed},
        client = SynthyraConnect[Authentication -> "secret"];
        directory = createTestDownloadDirectory[];
        path = FileNameJoin[{directory, "fixture.zip"}];
        writeTestBytes[path, {99}];
        result = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout}, httpCalls++; $Failed],
                Synthyra`SynthyraLink`Private`$SynthyraDownloadTransport = Function[{request, temporaryPath, timeout},
                    Module[{actualPath = temporaryPath <> "-1"},
                        downloadCalls++;
                        writeTestBytes[actualPath, {1, 2, 3}];
                        <|"File" -> File[actualPath], "StatusCode" -> 200, "Headers" -> {}, "ContentType" -> "application/zip"|>
                    ]
                ]
            },
            client["DownloadExample", <||>, Target -> File[path]]
        ];
        observed = {result === File[path], readTestBytes[path], httpCalls, downloadCalls, Length[FileNames[All, directory]]};
        DeleteDirectory[directory, DeleteContents -> True];
        observed
    ],
    {True, {1, 2, 3}, 0, 1, 1},
    TestID -> "file-target-uses-returned-path-and-commits-on-success@@Tests/Wolfram/Runtime.wlt:495,1-521,2"
];

VerificationTest[
    Module[{client, calls = 0, directory, path, result, observed},
        client = SynthyraConnect[Authentication -> "secret"];
        directory = createTestDownloadDirectory[];
        path = FileNameJoin[{directory, "retry.zip"}];
        result = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraDownloadTransport = Function[{request, temporaryPath, timeout},
                    calls++;
                    If[calls === 1,
                        writeTestBytes[temporaryPath, Normal @ StringToByteArray["{\"detail\":\"retry\"}", "UTF8"]];
                        <|"File" -> File[temporaryPath], "StatusCode" -> 503, "Headers" -> {}, "ContentType" -> "application/json"|>,
                        writeTestBytes[temporaryPath, {4, 5, 6}];
                        <|"File" -> File[temporaryPath], "StatusCode" -> 200, "Headers" -> {}, "ContentType" -> "application/zip"|>
                    ]
                ],
                Synthyra`SynthyraLink`Private`$SynthyraPauseFunction = Function[Null]
            },
            client["DownloadExample", <||>, Target -> File[path]]
        ];
        observed = {result === File[path], readTestBytes[path], calls, Length[FileNames[All, directory]]};
        DeleteDirectory[directory, DeleteContents -> True];
        observed
    ],
    {True, {4, 5, 6}, 2, 1},
    TestID -> "streaming-download-retry-cleans-transient-body@@Tests/Wolfram/Runtime.wlt:523,1-550,2"
];

VerificationTest[
    Module[{client, directory, path, failure, observed},
        client = SynthyraConnect[Authentication -> "secret"];
        directory = createTestDownloadDirectory[];
        path = FileNameJoin[{directory, "preserve.zip"}];
        writeTestBytes[path, {7, 8, 9}];
        failure = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraDownloadTransport = Function[{request, temporaryPath, timeout},
                    writeTestBytes[temporaryPath, Normal @ StringToByteArray["{\"detail\":\"download failed\"}", "UTF8"]];
                    <|
                        "File" -> File[temporaryPath], "StatusCode" -> 500,
                        "Headers" -> {"X-Synthyra-Request-ID" -> "download-request-1"},
                        "ContentType" -> "application/json"
                    |>
                ]
            },
            client["DownloadExample", <||>, Target -> File[path], MaxRetries -> 0]
        ];
        observed = {
            FailureQ[failure], readTestBytes[path], Length[FileNames[All, directory]],
            ! FreeQ[failure, "download-request-1"], ! FreeQ[failure, "download failed"]
        };
        DeleteDirectory[directory, DeleteContents -> True];
        observed
    ],
    {True, {7, 8, 9}, 1, True, True},
    TestID -> "failed-streaming-download-preserves-target-and-error-body@@Tests/Wolfram/Runtime.wlt:552,1-581,2"
];

VerificationTest[
    Module[{client, calls = 0, responses, result},
        client = SynthyraConnect[Authentication -> "secret"];
        responses = {
            mockResponse[202, "{\"jobId\":\"job-1\",\"state\":\"queued\"}"],
            mockResponse[200, "{\"jobId\":\"job-1\",\"state\":\"queued\"}"],
            mockResponse[200, "{\"jobId\":\"job-1\",\"state\":\"succeeded\"}"],
            mockResponse[200, "{\"result\":\"ready\"}"]
        };
        result = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedManifest = $testManifest,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout}, calls++; responses[[calls]]],
                Synthyra`SynthyraLink`Private`$SynthyraPauseFunction = Function[Null]
            },
            SynthyraJobResult[
                client["SubmitExample", <|"proteinIds" -> {"P1", "P2"}|>],
                PollInterval -> 0., Timeout -> 10.
            ]
        ];
        {result, calls}
    ],
    {<|"result" -> "ready"|>, 4},
    TestID -> "async-success-transition-and-result@@Tests/Wolfram/Runtime.wlt:583,1-609,2"
];

VerificationTest[
    Module[
        {client, calls = 0, downloads = 0, responses, artifactRequest = None, result, signedURL},
        signedURL = "https://artifacts.example.invalid/result.json?signature=unit-test-capability";
        client = SynthyraConnect[Authentication -> "secret"];
        responses = {
            mockResponse[202, "{\"jobId\":\"job-artifact\",\"state\":\"queued\"}"],
            mockResponse[200, "{\"jobId\":\"job-artifact\",\"state\":\"succeeded\",\"output_url\":\"" <> signedURL <> "\"}"],
            mockResponse[500, "{\"detail\":\"result too large\"}", "application/json", {"X-Synthyra-Request-ID" -> "gateway-result-1"}]
        };
        result = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedManifest = $testManifest,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout}, calls++; responses[[calls]]],
                Synthyra`SynthyraLink`Private`$SynthyraDownloadTransport = Function[{request, temporaryPath, timeout},
                    downloads++;
                    artifactRequest = request;
                    writeTestBytes[temporaryPath, Normal @ StringToByteArray["{\"result\":\"artifact-ready\"}", "UTF8"]];
                    <|
                        "File" -> File[temporaryPath], "StatusCode" -> 200,
                        "Headers" -> {"X-Synthyra-Request-ID" -> "artifact-result-1"},
                        "ContentType" -> "application/json"
                    |>
                ],
                Synthyra`SynthyraLink`Private`$SynthyraPauseFunction = Function[Null]
            },
            SynthyraJobResult[
                client["SubmitExample", <|"proteinIds" -> {"P1", "P2"}|>],
                PollInterval -> 0., Timeout -> 10., MaxRetries -> 0
            ]
        ];
        {
            result,
            calls,
            downloads,
            FreeQ[artifactRequest[[2, "Headers"]], "Authorization"],
            MemberQ[artifactRequest[[2, "Headers"]], "User-Agent" -> ("SynthyraLink/" <> Synthyra`SynthyraLink`Private`$SynthyraPacletVersion)]
        }
    ],
    {<|"result" -> "artifact-ready"|>, 3, 1, True, True},
    TestID -> "async-result-falls-back-to-signed-artifact-without-authorization@@Tests/Wolfram/Runtime.wlt:611,1-654,2"
];

VerificationTest[
    Module[{client, calls = 0, responses, job, status, signedURL},
        signedURL = "https://artifacts.example.invalid/result.json?signature=public-status-redaction";
        client = SynthyraConnect[Authentication -> "secret"];
        responses = {
            mockResponse[202, "{\"jobId\":\"job-status\",\"state\":\"queued\"}"],
            mockResponse[200, "{\"jobId\":\"job-status\",\"state\":\"succeeded\",\"output_url\":\"" <> signedURL <> "\"}"]
        };
        status = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedManifest = $testManifest,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout}, calls++; responses[[calls]]]
            },
            job = client["SubmitExample", <|"proteinIds" -> {"P1", "P2"}|>];
            SynthyraJobStatus[job]
        ];
        {Lookup[status, "output_url", Missing["NotAvailable"]], FreeQ[status, signedURL]}
    ],
    {"<redacted>", True},
    TestID -> "public-job-status-redacts-signed-artifact-url@@Tests/Wolfram/Runtime.wlt:656,1-678,2"
];

VerificationTest[
    Module[{client, job, signedURL},
        signedURL = "https://artifacts.example.invalid/result.json?signature=object-redaction";
        client = SynthyraConnect[Authentication -> "secret"];
        job = SynthyraJobObject[
            <|
                "Client" -> client,
                "SubmissionOperation" -> "SubmitExample",
                "JobID" -> "job-object",
                "Job" -> <||>,
                "Submission" -> <|"output_url" -> signedURL|>,
                "LastStatus" -> <|"state" -> "succeeded", "output_url" -> signedURL|>
            |>
        ];
        {
            Lookup[Normal[job]["LastStatus"], "output_url", Missing["NotAvailable"]],
            FreeQ[Normal[job], signedURL],
            FreeQ[MakeBoxes[job, StandardForm], signedURL]
        }
    ],
    {"<redacted>", True, True},
    TestID -> "job-normal-and-boxes-redact-signed-artifact-url@@Tests/Wolfram/Runtime.wlt:680,1-702,2"
];

VerificationTest[
    Module[{client, calls = 0, downloads = 0, responses, failure, signedURL},
        signedURL = "https://artifacts.example.invalid/result.json?signature=no-auth-bypass";
        client = SynthyraConnect[Authentication -> "secret"];
        responses = {
            mockResponse[202, "{\"jobId\":\"job-auth\",\"state\":\"queued\"}"],
            mockResponse[200, "{\"jobId\":\"job-auth\",\"state\":\"succeeded\",\"output_url\":\"" <> signedURL <> "\"}"],
            mockResponse[401, "{\"detail\":\"unauthorized\"}"]
        };
        failure = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedManifest = $testManifest,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout}, calls++; responses[[calls]]],
                Synthyra`SynthyraLink`Private`$SynthyraDownloadTransport = Function[{request, temporaryPath, timeout}, downloads++; $Failed]
            },
            SynthyraJobResult[
                client["SubmitExample", <|"proteinIds" -> {"P1", "P2"}|>],
                PollInterval -> 0., Timeout -> 10., MaxRetries -> 0
            ]
        ];
        {FailureQ[failure], First[failure], downloads}
    ],
    {True, "SynthyraHTTPError", 0},
    TestID -> "result-auth-failure-does-not-use-signed-artifact@@Tests/Wolfram/Runtime.wlt:704,1-730,2"
];

VerificationTest[
    Module[{client, calls = 0, downloads = 0, responses, failure, unsafeURL},
        unsafeURL = "http://artifacts.example.invalid/result.json?signature=must-not-be-used";
        client = SynthyraConnect[Authentication -> "secret"];
        responses = {
            mockResponse[202, "{\"jobId\":\"job-unsafe\",\"state\":\"queued\"}"],
            mockResponse[200, "{\"jobId\":\"job-unsafe\",\"state\":\"succeeded\",\"output_url\":\"" <> unsafeURL <> "\"}"],
            mockResponse[500, "{\"detail\":\"result too large\"}"]
        };
        failure = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedManifest = $testManifest,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout}, calls++; responses[[calls]]],
                Synthyra`SynthyraLink`Private`$SynthyraDownloadTransport = Function[{request, temporaryPath, timeout}, downloads++; $Failed]
            },
            SynthyraJobResult[
                client["SubmitExample", <|"proteinIds" -> {"P1", "P2"}|>],
                PollInterval -> 0., Timeout -> 10., MaxRetries -> 0
            ]
        ];
        {
            FailureQ[failure], First[failure], downloads, FreeQ[failure, unsafeURL],
            Lookup[Lookup[failure[[2]], "ArtifactFailure", <||>], "Type", Missing["NotAvailable"]]
        }
    ],
    {True, "JobResultRetrievalFailed", 0, True, "InvalidSignedArtifactURL"},
    TestID -> "non-https-signed-artifact-fails-closed@@Tests/Wolfram/Runtime.wlt:732,1-761,2"
];

VerificationTest[
    Module[{client, calls = 0, downloads = 0, responses, failure, signedURL},
        signedURL = "https://artifacts.example.invalid/result.json?signature=failure-redaction";
        client = SynthyraConnect[Authentication -> "secret"];
        responses = {
            mockResponse[202, "{\"jobId\":\"job-failure\",\"state\":\"queued\"}"],
            mockResponse[200, "{\"jobId\":\"job-failure\",\"state\":\"succeeded\",\"output_url\":\"" <> signedURL <> "\"}"],
            mockResponse[500, "{\"detail\":\"gateway failure\"}", "application/json", {"X-Synthyra-Request-ID" -> "gateway-failure-id"}]
        };
        failure = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedManifest = $testManifest,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout}, calls++; responses[[calls]]],
                Synthyra`SynthyraLink`Private`$SynthyraDownloadTransport = Function[{request, temporaryPath, timeout},
                    downloads++;
                    writeTestBytes[temporaryPath, Normal @ StringToByteArray["{\"detail\":\"artifact failure\"}", "UTF8"]];
                    <|
                        "File" -> File[temporaryPath], "StatusCode" -> 503,
                        "Headers" -> {"X-Synthyra-Request-ID" -> "artifact-failure-id"},
                        "ContentType" -> "application/json"
                    |>
                ]
            },
            SynthyraJobResult[
                client["SubmitExample", <|"proteinIds" -> {"P1", "P2"}|>],
                PollInterval -> 0., Timeout -> 10., MaxRetries -> 0
            ]
        ];
        {
            FailureQ[failure], First[failure], FreeQ[failure, signedURL],
            Lookup[Lookup[failure[[2]], "GatewayFailure", <||>], "RequestID", Missing["NotAvailable"]],
            Lookup[Lookup[failure[[2]], "ArtifactFailure", <||>], "RequestID", Missing["NotAvailable"]]
        }
    ],
    {True, "JobResultRetrievalFailed", True, "gateway-failure-id", "artifact-failure-id"},
    TestID -> "artifact-failure-redacts-url-and-preserves-request-identifiers@@Tests/Wolfram/Runtime.wlt:763,1-801,2"
];

VerificationTest[
    Module[{client, calls = 0, responses, result},
        client = SynthyraConnect[Authentication -> "secret"];
        responses = {
            mockResponse[202, "{\"jobId\":\"job-2\",\"state\":\"queued\"}"],
            mockResponse[200, "{\"jobId\":\"job-2\",\"state\":\"cancelled\"}"]
        };
        result = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedManifest = $testManifest,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout}, calls++; responses[[calls]]]
            },
            SynthyraCancelJob[client["SubmitExample", <|"proteinIds" -> {"P1", "P2"}|>]]
        ];
        {result, calls}
    ],
    {<|"jobId" -> "job-2", "state" -> "cancelled"|>, 2},
    TestID -> "declared-job-cancellation@@Tests/Wolfram/Runtime.wlt:803,1-823,2"
];

VerificationTest[
    Module[{payload, decoded},
        payload = <|
            "data" -> BaseEncode[ByteArray[{1, 2, 3, 4}]],
            "shape" -> {2, 2}, "row_ids" -> {"A", "B"}, "column_ids" -> {"C", "D"}
        |>;
        decoded = SynthyraScoreMatrix[payload];
        {Normal[decoded["Matrix"]], decoded["RowIDs"], decoded["ColumnIDs"]}
    ],
    {{{1, 2}, {3, 4}}, {"A", "B"}, {"C", "D"}},
    TestID -> "row-major-uint8-matrix@@Tests/Wolfram/Runtime.wlt:825,1-836,2"
];

VerificationTest[
    Module[{ids, payload, decoded},
        ids = {"Q1", "P1", "P2"};
        payload = <|
            "data" -> BaseEncode[ByteArray[Range[0, 8]]],
            "shape" -> {3, 3}, "row_ids" -> ids, "column_ids" -> ids,
            "query_ids" -> {"Q1"}, "reference_proteome_ids" -> {"P1", "P2"},
            "matrix_contract" -> "full_proteome"
        |>;
        decoded = SynthyraScoreMatrix[payload];
        {Dimensions[Normal[decoded["Matrix"]]], decoded["RowIDs"] === ids, decoded["ColumnIDs"] === ids}
    ],
    {{3, 3}, True, True},
    TestID -> "full-proteome-q-plus-p-square-matrix@@Tests/Wolfram/Runtime.wlt:838,1-852,2"
];

VerificationTest[
    FailureQ @ SynthyraScoreMatrix[<|
        "data" -> BaseEncode[ByteArray[{1, 2, 3, 4, 5, 6}]],
        "shape" -> {2, 3},
        "row_ids" -> {"Q1", "P1"},
        "column_ids" -> {"Q1", "P1", "P2"},
        "mode" -> "proteome"
    |>],
    True,
    TestID -> "proteome-mode-always-enforces-full-square-contract@@Tests/Wolfram/Runtime.wlt:854,1-864,2"
];

VerificationTest[
    Module[{payload, decoded},
        payload = <|
            "proteome_scores_b64" -> BaseEncode[ByteArray[{1, 2, 3}]],
            "proteome_ids" -> {"P1", "P2", "P3"},
            "candidate_matrix_b64" -> BaseEncode[ByteArray[{10, 20, 30, 40}]],
            "candidate_matrix_shape" -> {2, 2}, "candidate_ids" -> {"C1", "C2"}
        |>;
        decoded = SynthyraScoreMatrix[payload];
        {
            Normal[decoded["Matrix"]], decoded["RowIDs"], decoded["ColumnIDs"],
            Normal[decoded["ProteomeScores"]], decoded["ProteomeIDs"]
        }
    ],
    {{{10, 20}, {30, 40}}, {"C1", "C2"}, {"C1", "C2"}, {1, 2, 3}, {"P1", "P2", "P3"}},
    TestID -> "real-expansion-envelope-prefers-candidate-matrix@@Tests/Wolfram/Runtime.wlt:866,1-882,2"
];

VerificationTest[
    FailureQ @ SynthyraScoreMatrix[<|
        "proteome_scores_b64" -> BaseEncode[ByteArray[{42}]],
        "proteome_ids" -> {"P1"}
    |>],
    True,
    TestID -> "proteome-score-vector-is-not-inferred-as-a-square-matrix@@Tests/Wolfram/Runtime.wlt:884,1-891,2"
];

VerificationTest[
    Module[{decoded},
        decoded = SynthyraScoreMatrix[<|
            "candidate_matrix_b64" -> BaseEncode[ByteArray[{}]],
            "candidate_matrix_shape" -> {0, 0}, "candidate_ids" -> {}
        |>];
        {Normal[decoded["Matrix"]], decoded["Shape"], decoded["RowIDs"], decoded["ColumnIDs"]}
    ],
    {{}, {0, 0}, {}, {}},
    TestID -> "empty-candidate-matrix-preserves-declared-shape@@Tests/Wolfram/Runtime.wlt:893,1-903,2"
];

VerificationTest[
    FailureQ @ SynthyraScoreMatrix[<|
        "data" -> BaseEncode[ByteArray[{1, 2, 3}]],
        "shape" -> {2, 2}, "row_ids" -> {"A", "B"}, "column_ids" -> {"A", "B"}
    |>],
    True,
    TestID -> "matrix-byte-count-mismatch-fails@@Tests/Wolfram/Runtime.wlt:905,1-912,2"
];

VerificationTest[
    FailureQ @ SynthyraScoreMatrix[<|
        "data" -> BaseEncode[ByteArray[{1, 2, 3, 4}]],
        "shape" -> {2, 2},
        "row_ids" -> {"Q1", "P1"},
        "column_ids" -> {"Q1", "P1"},
        "proteome_ids" -> {"Q1", "P1"},
        "query_count" -> 1,
        "proteome_count" -> 2
    |>],
    True,
    TestID -> "full-proteome-counts-must-match-matrix-dimension@@Tests/Wolfram/Runtime.wlt:914,1-926,2"
];

VerificationTest[
    Module[{verbose, compact, graphA, graphB, edgeKey},
        verbose = <|
            "nodes" -> {
                <|"id" -> "P1", "name" -> "One", "organism" -> "human", "sequence_length" -> 10, "node_type" -> "protein"|>,
                <|"id" -> "P2", "name" -> "Two", "organism" -> "human", "sequence_length" -> 20, "node_type" -> "protein"|>
            },
            "edges" -> {<|"source" -> "P1", "target" -> "P2", "confidence" -> 80, "is_novel" -> True, "in_string" -> False, "in_biogrid" -> False, "in_biogrid_mv" -> False, "interaction_type" -> "ppi"|>}
        |>;
        compact = <|
            "nodes" -> {
                <|"id" -> "P1", "name" -> "One", "organism" -> "human", "sl" -> 10, "uid" -> "P1"|>,
                <|"id" -> "P2", "name" -> "Two", "organism" -> "human", "sl" -> 20, "uid" -> "P2"|>
            },
            "edges" -> {<|"i" -> 0, "j" -> 1, "s" -> 80, "st" -> False, "bg" -> False, "bgmv" -> False|>}
        |>;
        graphA = SynthyraGraph[verbose];
        graphB = SynthyraGraph[compact];
        edgeKey[g_] := Sort[Sort[ToString[#, InputForm] & /@ (List @@ #)] & /@ EdgeList[g]];
        {edgeKey[graphA] === edgeKey[graphB], UndirectedGraphQ[graphA], UndirectedGraphQ[graphB]}
    ],
    {True, True, True},
    TestID -> "verbose-and-compact-graphs-are-equivalent@@Tests/Wolfram/Runtime.wlt:928,1-951,2"
];

VerificationTest[
    Module[{payload, graph, subgraph, edges, confidences, stringEvidence, biogridEvidence},
        payload = <|
            "nodes" -> {
                <|"id" -> "Q", "node_type" -> "protein"|>,
                <|"id" -> "P1", "node_type" -> "protein"|>,
                <|"id" -> "P2", "node_type" -> "protein"|>
            },
            "edges" -> {
                <|"i" -> 0, "j" -> 1, "s" -> 96, "st" -> True, "bg" -> False, "bgmv" -> False|>,
                <|"i" -> 1, "j" -> 2, "s" -> 70, "st" -> False, "bg" -> True, "bgmv" -> False|>
            },
            "metadata" -> <|"task_type" -> "ppi", "fixture" -> True|>
        |>;
        graph = SynthyraGraph[payload];
        subgraph = Subgraph[
            graph,
            VertexList @ NeighborhoodGraph[graph, Protein["Q"], 2],
            AnnotationRules -> Inherited
        ];
        edges = EdgeList[subgraph];
        confidences = AnnotationValue[{subgraph, #}, "Confidence"] & /@ edges;
        stringEvidence = AnnotationValue[{subgraph, #}, "STRING"] & /@ edges;
        biogridEvidence = AnnotationValue[{subgraph, #}, "BioGRID"] & /@ edges;
        GraphQ[subgraph] && Sort[confidences] === {70, 96} &&
            ContainsAll[stringEvidence, {False, True}] &&
            ContainsAll[biogridEvidence, {False, True}] &&
            AllTrue[edges, AnnotationValue[{subgraph, #}, "InteractionType"] === "ppi" &] &&
            AllTrue[edges, AssociationQ @ AnnotationValue[{subgraph, #}, "OriginalPayload"] &] &&
            AssociationQ @ AnnotationValue[subgraph, "SynthyraMetadata"]
    ],
    True,
    TestID -> "compact-graph-annotations-survive-neighborhood-subgraph@@Tests/Wolfram/Runtime.wlt:953,1-986,2"
];

VerificationTest[
    Module[{failure},
        failure = SynthyraGraph[<|
            "nodes" -> {
                <|"id" -> "P1", "node_type" -> "protein"|>,
                <|"id" -> "P2", "node_type" -> "protein"|>
            },
            "edges" -> {
                <|
                    "target" -> "P2", "interaction_type" -> "ppi",
                    "private_payload" -> "do-not-leak-ppi"
                |>
            }
        |>];
        FailureQ[failure] && failure[[1]] === "InvalidNamedEdgeEndpoint" &&
            Lookup[failure[[2]], "Endpoint"] === "Source" &&
            FreeQ[failure, "do-not-leak-ppi"] &&
            FreeQ[failure, Protein[""] | Ligand[""]]
    ],
    True,
    TestID -> "verbose-ppi-edge-missing-source-fails-sanitized@@Tests/Wolfram/Runtime.wlt:988,1-1009,2"
];

VerificationTest[
    Module[{failure},
        failure = SynthyraGraph[<|
            "nodes" -> {
                <|"id" -> "P1", "node_type" -> "protein"|>,
                <|"id" -> "L1", "node_type" -> "ligand"|>
            },
            "edges" -> {
                <|
                    "source" -> "P1", "target" -> "   ",
                    "interaction_type" -> "pli",
                    "private_payload" -> "do-not-leak-pli"
                |>
            }
        |>];
        FailureQ[failure] && failure[[1]] === "InvalidNamedEdgeEndpoint" &&
            Lookup[failure[[2]], "Endpoint"] === "Target" &&
            FreeQ[failure, "do-not-leak-pli"] &&
            FreeQ[failure, Protein[""] | Ligand[""]]
    ],
    True,
    TestID -> "verbose-pli-edge-blank-target-fails-sanitized@@Tests/Wolfram/Runtime.wlt:1011,1-1033,2"
];

VerificationTest[
    Module[{graph},
        graph = SynthyraGraph[<|
            "nodes" -> {<|"id" -> "same", "node_type" -> "protein"|>, <|"id" -> "same", "node_type" -> "ligand", "smiles" -> "C"|>},
            "edges" -> {<|"i" -> 0, "j" -> 1, "s" -> 90, "interaction_type" -> "pli"|>}
        |>];
        ContainsAll[VertexList[graph], {Protein["same"], Ligand["same"]}]
    ],
    True,
    TestID -> "protein-ligand-identifiers-do-not-collide@@Tests/Wolfram/Runtime.wlt:1035,1-1045,2"
];

VerificationTest[
    Module[{graph},
        graph = SynthyraGraph[<|
            "proteins" -> {"same"}, "ligands" -> {"same"}, "ppiEdges" -> {},
            "pliEdges" -> {<|"sourceIndex" -> 0, "targetIndex" -> 0, "confidence" -> .9|>}
        |>];
        ContainsAll[VertexList[graph], {Protein["same"], Ligand["same"]}] && Length[EdgeList[graph]] === 1
    ],
    True,
    TestID -> "typed-compact-actome-protein-ligand-indices@@Tests/Wolfram/Runtime.wlt:1047,1-1057,2"
];

VerificationTest[
    {
        Synthyra`SynthyraLink`Private`confidenceCost[0.8, Automatic],
        Synthyra`SynthyraLink`Private`confidenceCost[80, Automatic],
        Synthyra`SynthyraLink`Private`confidenceCost[80, None]
    },
    {0.2, 0.2, Missing["NotAvailable"]},
    SameTest -> (Max[Abs[N[#1[[;; 2]]] - N[#2[[;; 2]]]]] < 10^-12 && #1[[3]] === #2[[3]] &),
    TestID -> "confidence-cost-is-scale-aware-and-opt-in@@Tests/Wolfram/Runtime.wlt:1059,1-1068,2"
];

VerificationTest[
    Module[{network, compact, payloads, graphs},
        network = <|
            "nodes" -> {
                <|"id" -> "P1", "node_type" -> "protein"|>,
                <|"id" -> "P2", "node_type" -> "protein"|>
            },
            "edges" -> {<|"source" -> "P1", "target" -> "P2", "confidence" -> .8|>}
        |>;
        compact = <|
            "nodes" -> {
                <|"id" -> "P1"|>,
                <|"id" -> "P2"|>
            },
            "edges" -> {<|"i" -> 0, "j" -> 1, "s" -> 80|>}
        |>;
        payloads = {
            <|"result" -> network|>,
            <|"result_network" -> network|>,
            <|"actome_edges" -> compact|>,
            <|"result" -> <|"result_actome_edges" -> compact|>|>
        };
        graphs = SynthyraGraph /@ payloads;
        And @@ (GraphQ[#] && Length[EdgeList[#]] === 1 & /@ graphs)
    ],
    True,
    TestID -> "production-network-wrappers-unwrap-recursively@@Tests/Wolfram/Runtime.wlt:1070,1-1097,2"
];

VerificationTest[
    Module[{failure},
        failure = SynthyraGraph[<|"status" -> "complete", "result_envelope" -> <||>|>];
        FailureQ[failure] && failure[[1]] === "MissingNetworkPayload"
    ],
    True,
    TestID -> "nonnetwork-job-result-fails-instead-of-empty-graph@@Tests/Wolfram/Runtime.wlt:1099,1-1106,2"
];

VerificationTest[
    Module[{failure},
        failure = SynthyraGraph[<|
            "result" -> <|
                "nodes" -> {
                    <|"id" -> "P1", "node_type" -> "protein"|>,
                    <|"id" -> "P2", "node_type" -> "protein"|>
                },
                "edges" -> {<|"source" -> "P1", "target" -> "P2", "score" -> 80, "distance" -> 0.2|>},
                "html_format" -> "srcdoc",
                "focus" -> <|"query_ids" -> {"P1"}|>,
                "metadata" -> <|"layout_source" -> "lhallee/tmap"|>
            |>
        |>];
        FailureQ[failure] && failure[[1]] === "TmapTreeIsNotInteractionNetwork"
    ],
    True,
    TestID -> "tmap-layout-tree-is-not-an-interaction-network@@Tests/Wolfram/Runtime.wlt:1108,1-1126,2"
];

VerificationTest[
    Module[{failure},
        failure = SynthyraGraph[<|
            "nodes" -> {
                <|"id" -> "   ", "private_payload" -> "do-not-leak-node"|>,
                <|"id" -> "P2"|>
            },
            "edges" -> {<|"i" -> 0, "j" -> 1, "s" -> 50|>}
        |>];
        FailureQ[failure] && failure[[1]] === "InvalidNetworkNodeIdentifier" &&
            FreeQ[failure, "do-not-leak-node"] && FreeQ[failure, Protein[""] | Ligand[""]]
    ],
    True,
    TestID -> "blank-network-node-identifier-fails-sanitized@@Tests/Wolfram/Runtime.wlt:1128,1-1142,2"
];

VerificationTest[
    MissingQ @ Synthyra`SynthyraLink`Private`edgeNovelty[<|"i" -> 0, "j" -> 1, "s" -> 80|>],
    True,
    TestID -> "compact-edge-without-evidence-has-unknown-novelty@@Tests/Wolfram/Runtime.wlt:1144,1-1148,2"
];

VerificationTest[
    Module[{data},
        data = SynthyraHypergraphData[<|
            "vertices" -> {<|"id" -> "P1", "node_type" -> "protein"|>, <|"id" -> "L1", "node_type" -> "ligand"|>},
            "hyperedges" -> {<|"id" -> "complex-1", "members" -> {<|"id" -> "P1", "node_type" -> "protein"|>, <|"id" -> "L1", "node_type" -> "ligand"|>}, "confidence" -> .9|>}
        |>];
        {data["Vertices"], data["MemberLists"], KeyExistsQ[data, "OriginalPayload"]}
    ],
    {{Protein["P1"], Ligand["L1"]}, {{Protein["P1"], Ligand["L1"]}}, True},
    TestID -> "lossless-hypergraph-member-lists@@Tests/Wolfram/Runtime.wlt:1150,1-1160,2"
];

$coordinatedWorkbenchFixture = <|
    "dfa" -> <|
        "protein_id" -> "P04637",
        "sequence" -> "MEEPQSDPSV",
        "plddt" -> 82.5,
        "pdb_string" -> "HEADER    SYNTHETIC TEST STRUCTURE",
        "oracle_predictions" -> {
            <|"oracle_name" -> "solubility", "score_mode" -> "binary_prob", "score" -> .8|>,
            <|
                "oracle_name" -> "localization", "score_mode" -> "multiclass_prob",
                "label_names" -> {"nucleus", "cytoplasm"}, "score" -> {.7, .3}
            |>
        },
        "camp_annotations" -> {<|"annotation_id" -> "CAMP:1", "score" -> .9|>},
        "translator_annotations" -> {<|"annotation_id" -> "GO:1", "confidence" -> .7|>}
    |>,
    "network" -> <|
        "actome_edges" -> <|
            "nodes" -> {<|"id" -> "P04637"|>, <|"id" -> "Q00987"|>},
            "edges" -> {<|"i" -> 0, "j" -> 1, "s" -> 91, "st" -> True, "bg" -> False, "bgmv" -> False|>},
            "query_ids" -> {"P04637"},
            "node_tiers" -> <|"P04637" -> 0, "Q00987" -> 1|>,
            "subnetwork_heatmap" -> <|
                "ids" -> {"P04637", "Q00987"}, "scores" -> {{100, 91}, {91, 100}},
                "cluster_order" -> {0, 1}, "query_indices" -> {0}
            |>
        |>,
        "tmap_tree" -> <|
            "nodes" -> {
                <|"id" -> "P04637", "x" -> 0., "y" -> 0.|>,
                <|"id" -> "Q00987", "x" -> 1., "y" -> 1.|>
            },
            "edges" -> {<|"source" -> "P04637", "target" -> "Q00987", "score" -> 91, "distance" -> .09|>},
            "focus" -> <|"query_ids" -> {"P04637"}|>,
            "metadata" -> <|"layout_source" -> "fixture"|>
        |>
    |>,
    "enrichment" -> <|
        "terms" -> {
            <|
                "term" -> "p53 binding", "library" -> "GO_Molecular_Function",
                "p_value" -> 1.*^-6, "adjusted_p_value" -> 2.*^-5,
                "genes" -> {"P04637", "Q00987"}, "gene_count" -> 2
            |>
        }
    |>,
    "api_key" -> "must-not-be-retained"
|>;

VerificationTest[
    Module[{analysis, normal},
        analysis = SynthyraAnalysis[$coordinatedWorkbenchFixture];
        normal = Normal[analysis];
        {
            Head[analysis],
            analysis["ProteinID"],
            Length[analysis["OraclePredictions"]],
            Length[Lookup[analysis["ActomeEdges"], "edges", {}]],
            AssociationQ[analysis["TMAPTree"]],
            FreeQ[normal, "must-not-be-retained"]
        }
    ],
    {SynthyraAnalysisObject, "P04637", 2, 1, True, True},
    TestID -> "coordinated-result-normalizes-all-primary-families-and-redacts-credentials@@Tests/Wolfram/Runtime.wlt:1211,1-1226,2"
];

VerificationTest[
    Module[{analysis},
        analysis = SynthyraAnalysis[<|
            "result_esmfold" -> <|"plddt" -> 77.2, "pdb_string" -> "HEADER"|>,
            "result_oracles" -> <|"oracle_predictions" -> {<|"oracle_name" -> "solubility", "score" -> .6|>}|>,
            "result_dfa_camp" -> {<|"annotations" -> {
                <|"annotation_id" -> "CAMP:partial-1"|>,
                <|"annotation_id" -> "CAMP:partial-2"|>
            }|>},
            "result_dfa_translator" -> <|"annotations" -> {<|"annotation_id" -> "GO:partial"|>}|>,
            "result_network" -> <|
                "network" -> <|
                    "nodes" -> {<|"id" -> "P1"|>, <|"id" -> "P2"|>},
                    "edges" -> {<|"source" -> "P1", "target" -> "P2", "confidence" -> .8|>}
                |>,
                "actome_edges" -> <|
                    "nodes" -> {<|"id" -> "P1"|>, <|"id" -> "P2"|>},
                    "edges" -> {<|"i" -> 0, "j" -> 1, "s" -> 80|>},
                    "query_ids" -> {"P1"}, "node_tiers" -> <|"P1" -> 0, "P2" -> 1|>
                |>
            |>
        |>];
        {
            Lookup[analysis["DFA"], "plddt"],
            Length[analysis["OraclePredictions"]],
            Length[analysis["CAMPAnnotations"]],
            Length[analysis["TranslatorAnnotations"]],
            AssociationQ[analysis["ActomeEdges"]],
            AssociationQ[analysis["BiologicalNetwork"]]
        }
    ],
    {77.2, 1, 2, 1, True, True},
    TestID -> "progressive-coordinated-fields-normalize-before-final-result@@Tests/Wolfram/Runtime.wlt:1228,1-1261,2"
];

VerificationTest[
    Module[{analysis},
        analysis = SynthyraAnalysis[$coordinatedWorkbenchFixture];
        {
            Normal @ SynthyraOracleDataset[analysis],
            Normal @ SynthyraEnrichmentDataset[analysis]
        }
    ],
    {
        {
            <|"Oracle" -> "solubility", "oracle_name" -> "solubility", "score_mode" -> "binary_prob", "score" -> .8|>,
            <|
                "Oracle" -> "localization", "oracle_name" -> "localization", "score_mode" -> "multiclass_prob",
                "label_names" -> {"nucleus", "cytoplasm"}, "score" -> {.7, .3}
            |>
        },
        {
            <|
                "term" -> "p53 binding", "library" -> "GO_Molecular_Function",
                "p_value" -> 1.*^-6, "adjusted_p_value" -> 2.*^-5,
                "genes" -> {"P04637", "Q00987"}, "gene_count" -> 2
            |>
        }
    },
    TestID -> "workbench-oracle-and-enrichment-datasets-preserve-service-fields@@Tests/Wolfram/Runtime.wlt:1263,1-1288,2"
];

VerificationTest[
    Module[{analysis, graph},
        analysis = SynthyraAnalysis[$coordinatedWorkbenchFixture];
        graph = SynthyraGraph[analysis["ActomeEdges"]];
        GraphQ[graph] && Length[EdgeList[graph]] === 1 &&
            FailureQ[SynthyraGraph[analysis["TMAPTree"]]]
    ],
    True,
    TestID -> "workbench-keeps-biological-actome-separate-from-tmap-layout-tree@@Tests/Wolfram/Runtime.wlt:1290,1-1299,2"
];

VerificationTest[
    Module[{graph, subset},
        graph = SynthyraGraph[<|
            "nodes" -> (<|"id" -> #|> & /@ {"A", "B", "C", "D", "E"}),
            "edges" -> {
                <|"source" -> "A", "target" -> "B", "confidence" -> .95|>,
                <|"source" -> "A", "target" -> "C", "confidence" -> .90|>,
                <|"source" -> "B", "target" -> "D", "confidence" -> .99|>,
                <|"source" -> "C", "target" -> "E", "confidence" -> .80|>
            }
        |>];
        subset = Synthyra`SynthyraLink`Private`workbenchGraphSubset[
            graph,
            {Protein["A"]},
            0.,
            2,
            4
        ];
        VertexCount[subset] === 4 &&
            ContainsAll[VertexList[subset], Protein /@ {"A", "B", "C", "D"}] &&
            ! MemberQ[VertexList[subset], Protein["E"]]
    ],
    True,
    TestID -> "workbench-caps-ranked-query-neighborhood-with-paths-preserved@@Tests/Wolfram/Runtime.wlt:1301,1-1325,2"
];

VerificationTest[
    Module[{analysis, workbench},
        analysis = SynthyraAnalysis[$coordinatedWorkbenchFixture];
        workbench = Quiet @ SynthyraWorkbench[analysis];
        Head[workbench] === TabView && Length[First[workbench]] === 11
    ],
    True,
    TestID -> "workbench-builds-complete-tabbed-interface@@Tests/Wolfram/Runtime.wlt:1327,1-1335,2"
];

VerificationTest[
    Module[{client, seen = None},
        client = SynthyraConnect[Authentication -> "secret"];
        Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators,
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout}, seen = request; mockResponse[200, "{\"job_id\":\"job-7\"}"]]
            },
            client["SubmitExample", <|"ProteinIDs" -> {"P1", "P2"}|>]
        ];
        ImportString[seen[[2, "Body"]], "RawJSON"]
    ],
    <|"proteinIds" -> {"P1", "P2"}|>,
    TestID -> "loosely-matched-body-keys-are-sent-as-declared"
];

VerificationTest[
    Module[{client, failure},
        client = SynthyraConnect[Authentication -> "secret"];
        failure = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators
            },
            client["SubmitExample", <|"proteinIds" -> {"only-one"}|>]
        ];
        failure["Message"]
    ],
    "Invalid SubmitExample request: Body.proteinIds fewer than minItems.",
    TestID -> "validation-failure-message-names-field-and-reason"
];

VerificationTest[
    Module[{client, calls = 0, notices = {}, result},
        client = SynthyraConnect[Authentication -> "secret"];
        result = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators,
                Synthyra`SynthyraLink`Private`$SynthyraPauseFunction = Function[delay, Null],
                Synthyra`SynthyraLink`Private`$SynthyraRetryNotifier = Function[{operation, reason, delay}, AppendTo[notices, {reason, delay}]],
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout},
                    calls++;
                    If[calls === 1,
                        mockResponse[503, "{\"detail\":\"Service unavailable.\",\"error\":\"warming_up\",\"retry_after_s\":30}", "application/json", {"Retry-After" -> "30"}],
                        mockResponse[200, "{\"job_id\":\"job-8\"}"]
                    ]
                ]
            },
            client["SubmitExample", <|"proteinIds" -> {"P1", "P2"}|>]
        ];
        {calls, notices, Head[result]}
    ],
    {2, {{"warming up", 30.}}, SynthyraJobObject},
    TestID -> "post-retried-when-gateway-reports-warming-up"
];

VerificationTest[
    Module[{client, calls = 0, result},
        client = SynthyraConnect[Authentication -> "secret"];
        result = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators,
                Synthyra`SynthyraLink`Private`$SynthyraPauseFunction = Function[delay, Null],
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout},
                    calls++; mockResponse[503, "{\"detail\":\"Service unavailable.\"}"]
                ]
            },
            client["SubmitExample", <|"proteinIds" -> {"P1", "P2"}|>]
        ];
        {calls, result["StatusCode"], result["Message"]}
    ],
    {1, 503, "Synthyra SubmitExample failed with HTTP 503: Service unavailable."},
    TestID -> "post-not-retried-on-an-unexplained-503"
];

VerificationTest[
    Module[{client, job, timeouts = {}, result},
        client = SynthyraConnect[Authentication -> "secret"];
        result = Block[
            {
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedOperations = $testOperations,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedManifest = $testManifest,
                Synthyra`SynthyraLink`Private`$SynthyraGeneratedValidatorSpecifications = $testValidators,
                Synthyra`SynthyraLink`Private`$SynthyraPauseFunction = Function[delay, Null],
                Synthyra`SynthyraLink`Private`$SynthyraHTTPTransport = Function[{request, timeout},
                    AppendTo[timeouts, timeout];
                    Which[
                        StringEndsQ[First[request], "/jobs"], mockResponse[200, "{\"job_id\":\"job-9\"}"],
                        StringEndsQ[First[request], "/result"], mockResponse[200, "{\"value\":1}"],
                        True, mockResponse[200, "{\"status\":\"succeeded\"}"]
                    ]
                ]
            },
            job = client["SubmitExample", <|"proteinIds" -> {"P1", "P2"}|>];
            SynthyraJobResult[job, Timeout -> 3600]
        ];
        {result, Max[timeouts] < 3600}
    ],
    {<|"value" -> 1|>, True},
    TestID -> "job-wait-timeout-is-not-the-http-timeout"
];
