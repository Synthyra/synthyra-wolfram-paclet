(* Native HTTP runtime. This file is loaded inside Synthyra`SynthyraLink`Private`. *)

$SynthyraProductionURL = "https://api.synthyra.com";
$SynthyraDevelopmentURL = "https://apidev.synthyra.com";
$SynthyraCredentialName = "Synthyra/APIKey";
$SynthyraCredentialVault = <||>;
$SynthyraGeneratedOperations = <||>;
$SynthyraGeneratedManifest = <||>;
$SynthyraGeneratedSchemas = <||>;
$SynthyraGeneratedValidatorSpecifications = <||>;
$SynthyraGeneratedSerializerSpecifications = <||>;

loadGeneratedMetadata[] := Module[{loader},
    loader = FileNameJoin[{$SynthyraKernelDirectory, "Generated", "Loader.wl"}];
    If[FileExistsQ[loader],
        Quiet[Check[Get[loader], Null]]
    ];
    If[! AssociationQ[$SynthyraGeneratedOperations],
        $SynthyraGeneratedOperations = <||>
    ];
    If[! AssociationQ[$SynthyraGeneratedManifest],
        $SynthyraGeneratedManifest = <||>
    ];
];

loadGeneratedMetadata[];

canonicalName[value_] := ToLowerCase @ StringReplace[
    Which[
        StringQ[value], value,
        Head[Unevaluated[value]] === Symbol, SymbolName[Unevaluated[value]],
        True, ToString[value, InputForm]
    ],
    {"_" -> "", "-" -> "", " " -> ""}
];

(* The value of the first of `names` present in `assoc`, matching keys by canonicalName; the
   order of `names` is the priority. *)
associationLookup[assoc_Association, names_List, default_: Missing["NotAvailable"]] := Module[
    {byName, match},
    byName = GroupBy[Normal[assoc], canonicalName[First[#]] &, First];
    match = SelectFirst[canonicalName /@ names, KeyExistsQ[byName, #] &, Missing["NotFound"]];
    If[MissingQ[match], default, Last[byName[match]]]
];
associationLookup[_, _, default_: Missing["NotAvailable"]] := default;

validCredentialString[value_] := StringQ[value] && StringLength[StringTrim[value]] > 0;

extractCredential[value_] := Module[{normal, candidate},
    Which[
        validCredentialString[value], StringTrim[value],
        AssociationQ[value],
            candidate = associationLookup[value, {"APIKey", "api_key", "Password", "Token"}];
            If[validCredentialString[candidate], StringTrim[candidate], Missing["InvalidCredential"]],
        Head[value] === SystemCredentialData,
            normal = Quiet[Check[Normal[value], $Failed]];
            If[AssociationQ[normal], extractCredential[normal], Missing["InvalidCredential"]],
        True, Missing["InvalidCredential"]
    ]
];

desktopFrontEndAvailableQ[] := TrueQ[$Notebooks] && Head[$FrontEnd] === FrontEndObject;

missingCredentialFailure[] := Failure[
    "MissingCredential",
    <|
        "MessageTemplate" -> "No Synthyra API credential is available. Set SYNTHYRA_API_KEY or SystemCredential[\"Synthyra/APIKey\"].",
        "CredentialName" -> $SynthyraCredentialName
    |>
];

resolveCredential[None] := None;
resolveCredential[explicit_] /; explicit =!= Automatic := Module[{credential},
    credential = extractCredential[explicit];
    If[validCredentialString[credential], credential, Failure[
        "InvalidCredential",
        <|"MessageTemplate" -> "Authentication must resolve to a nonempty API-key string."|>
    ]]
];
resolveCredential[Automatic] := Module[{candidate, dialog},
    candidate = extractCredential[Environment["SYNTHYRA_API_KEY"]];
    If[validCredentialString[candidate], Return[candidate]];

    candidate = extractCredential @ Quiet[Check[SystemCredential[$SynthyraCredentialName], Missing["NotAvailable"]]];
    If[validCredentialString[candidate], Return[candidate]];

    If[! desktopFrontEndAvailableQ[], Return[missingCredentialFailure[]]];

    dialog = Quiet @ Check[
        AuthenticationDialog[
            "Password",
            SystemCredentialKey -> $SynthyraCredentialName,
            AppearanceRules -> <|
                "Title" -> "Connect to Synthyra",
                "Description" -> "Enter a Synthyra API key. The field is masked and can be saved in secure system credential storage.",
                "SubmitLabel" -> "Connect"
            |>
        ],
        $Canceled
    ];
    If[dialog === $Canceled, Return[missingCredentialFailure[]]];
    candidate = extractCredential[dialog];
    If[validCredentialString[candidate], candidate, missingCredentialFailure[]]
];

storeCredential[None] := Missing["Anonymous"];
storeCredential[credential_String] := Module[{handle = CreateUUID["credential-"]},
    AssociateTo[$SynthyraCredentialVault, handle -> credential];
    handle
];

credentialForClient[SynthyraClientObject[data_Association]] := Module[{handle},
    handle = Lookup[data, "CredentialHandle", Missing["NotAvailable"]];
    If[MissingQ[handle],
        None,
        Lookup[$SynthyraCredentialVault, handle, missingCredentialFailure[]]
    ]
];

normalizeEnvironment[environment_] := Switch[canonicalName[environment],
    "production", "Production",
    "development", "Development",
    _, $Failed
];

defaultBaseURL["Production"] := $SynthyraProductionURL;
defaultBaseURL["Development"] := $SynthyraDevelopmentURL;

Options[SynthyraConnect] = {
    Authentication -> Automatic
};

(* Development serves the ESMFold2-300 fold default that production does not yet.
   TODO(paclet_production_default): make "Production" the default again once Synthyra/synth#80 is promoted. *)
$SynthyraDefaultEnvironment = "Development";

SynthyraConnect[opts : OptionsPattern[]] := SynthyraConnect[$SynthyraDefaultEnvironment, opts];

SynthyraConnect[environment_String, OptionsPattern[]] := Module[
    {normalizedEnvironment, credential, credentialHandle, url},
    normalizedEnvironment = normalizeEnvironment[environment];
    If[normalizedEnvironment === $Failed,
        Return @ Failure[
            "UnknownEnvironment",
            <|
                "MessageTemplate" -> "The Synthyra environment must be \"Production\" or \"Development\".",
                "Environment" -> environment
            |>
        ]
    ];

    credential = resolveCredential[OptionValue[Authentication]];
    If[FailureQ[credential], Return[credential]];
    credentialHandle = storeCredential[credential];

    url = defaultBaseURL[normalizedEnvironment];

    $SynthyraClient = SynthyraClientObject[
        <|
            "Environment" -> normalizedEnvironment,
            "BaseURL" -> url,
            "CredentialHandle" -> credentialHandle
        |>
    ]
];

SynthyraClientObject /: MakeBoxes[client : SynthyraClientObject[data_Association], form_] := With[
    {
        environment = Lookup[data, "Environment", "Unknown"],
        authenticated = ! MissingQ[Lookup[data, "CredentialHandle", Missing["Anonymous"]]]
    },
    InterpretationBox[
        RowBox[{
            "SynthyraClientObject", "[",
            ToBoxes[
                <|"Environment" -> environment, "Authentication" -> If[authenticated, "Configured", "Anonymous"]|>,
                form
            ],
            "]"
        }],
        client
    ]
];

SynthyraClientObject /: Normal[SynthyraClientObject[data_Association]] := <|
    "Environment" -> Lookup[data, "Environment", Missing["NotAvailable"]],
    "BaseURL" -> Lookup[data, "BaseURL", Missing["NotAvailable"]],
    "Authentication" -> If[
        MissingQ[Lookup[data, "CredentialHandle", Missing["Anonymous"]]],
        "Anonymous",
        "Configured"
    ]
|>;

operationMetadata[] := If[AssociationQ[$SynthyraGeneratedOperations], $SynthyraGeneratedOperations, <||>];

operationInformation[name_String] := Lookup[
    operationMetadata[],
    name,
    Failure[
        "UnknownOperation",
        <|
            "MessageTemplate" -> "The requested Synthyra operation is not present in the reviewed contract snapshot.",
            "Operation" -> name
        |>
    ]
];

SynthyraClientObject[data_Association]["Operations"] := Sort[Keys[operationMetadata[]]];
SynthyraClientObject[data_Association]["OperationInformation", name_String] := operationInformation[name];
SynthyraClientObject[data_Association][name_String] := SynthyraExecute[SynthyraClientObject[data], name, <||>];
optionRuleQ[expression_] := MatchQ[Unevaluated[expression], _Rule | _RuleDelayed];
nonOptionRequestQ[expression_] := ! optionRuleQ[expression];
SynthyraClientObject[data_Association][
    name_String,
    firstOption : (_Rule | _RuleDelayed),
    opts : OptionsPattern[SynthyraExecute]
] := SynthyraExecute[SynthyraClientObject[data], name, <||>, firstOption, opts];
SynthyraClientObject[data_Association][name_String, request_?nonOptionRequestQ, opts : OptionsPattern[SynthyraExecute]] :=
    SynthyraExecute[SynthyraClientObject[data], name, request, opts];

resolveOperationName[name_String] := Which[
    KeyExistsQ[operationMetadata[], name], name,
    KeyExistsQ[$SynthyraGeneratedManifest, name], $SynthyraGeneratedManifest[name],
    True, name
];

requestParts[operation_Association, request_] := Module[{parameters, body, hasBody},
    hasBody = With[{requestBody = associationLookup[operation, {"RequestBody"}, Missing["NotAvailable"]]},
        ! MissingQ[requestBody] && requestBody =!= Null
    ];
    If[
        AssociationQ[request] && (
            ! MissingQ[associationLookup[request, {"Parameters"}]] ||
            ! MissingQ[associationLookup[request, {"Body"}]]
        ),
        parameters = associationLookup[request, {"Parameters"}, <||>];
        body = associationLookup[request, {"Body"}, Missing["NoBody"]],
        parameters = If[! hasBody && AssociationQ[request], request, <||>];
        body = If[hasBody, request, Missing["NoBody"]]
    ];
    If[! AssociationQ[parameters],
        Return @ Failure[
            "InvalidParameters",
            <|"MessageTemplate" -> "The Parameters member must be an Association."|>
        ]
    ];
    If[! hasBody && ! MissingQ[body],
        Return @ Failure[
            "UnexpectedRequestBody",
            <|"MessageTemplate" -> "The reviewed operation contract does not allow a request body."|>
        ]
    ];
    If[! hasBody && ! AssociationQ[request] && request =!= Automatic,
        Return @ Failure[
            "InvalidParameters",
            <|"MessageTemplate" -> "A parameter-only operation requires an Association request."|>
        ]
    ];
    <|"Parameters" -> parameters, "Body" -> body|>
];

parameterScalar[value_] := Which[
    TrueQ[value], "true",
    value === False, "false",
    StringQ[value], value,
    NumericQ[value], ToString[value, InputForm],
    AssociationQ[value] || ListQ[value], $Failed,
    True, ToString[value, InputForm]
];

scalarListString[values_List, separator_String : ","] := Module[{serialized = parameterScalar /@ values},
    If[MemberQ[serialized, $Failed], $Failed, StringRiffle[serialized, separator]]
];

serializeQueryParameter[specification_Association, name_String, value_] := Module[
    {style, explode, serialized, pairs},
    style = canonicalName @ associationLookup[specification, {"Style"}, "form"];
    explode = TrueQ @ associationLookup[specification, {"Explode"}, style === "form"];
    If[style =!= "form", Return[$Failed]];
    Which[
        AssociationQ[value] && explode,
            pairs = Normal[value];
            If[AnyTrue[pairs, parameterScalar[Last[#]] === $Failed &], $Failed,
                (ToString[First[#]] -> parameterScalar[Last[#]]) & /@ pairs
            ],
        AssociationQ[value] && ! explode,
            pairs = Flatten[({ToString[First[#]], Last[#]} &) /@ Normal[value]];
            serialized = scalarListString[pairs];
            If[serialized === $Failed, $Failed, {name -> serialized}],
        ListQ[value] && explode,
            serialized = parameterScalar /@ value;
            If[MemberQ[serialized, $Failed], $Failed, (name -> #) & /@ serialized],
        ListQ[value] && ! explode,
            serialized = scalarListString[value];
            If[serialized === $Failed, $Failed, {name -> serialized}],
        True,
            serialized = parameterScalar[value];
            If[serialized === $Failed, $Failed, {name -> serialized}]
    ]
];

serializeSimpleParameter[specification_Association, value_] := Module[{explode, pairs, values},
    explode = TrueQ @ associationLookup[specification, {"Explode"}, False];
    Which[
        AssociationQ[value] && explode,
            pairs = Normal[value];
            values = parameterScalar[Last[#]] & /@ pairs;
            If[MemberQ[values, $Failed], $Failed,
                StringRiffle[MapThread[ToString[First[#1]] <> "=" <> #2 &, {pairs, values}], ","]
            ],
        AssociationQ[value] && ! explode,
            values = Flatten[({ToString[First[#]], Last[#]} &) /@ Normal[value]];
            scalarListString[values],
        ListQ[value], scalarListString[value],
        True, parameterScalar[value]
    ]
];

parameterValue[parameters_Association, name_String] := associationLookup[parameters, {name}, Missing["NotProvided"]];

serializeParameters[operation_Association, supplied_Association] := Module[
    {specifications, path, query = {}, headers = {}, name, location, required, value, missing, serializedValue},
    specifications = associationLookup[operation, {"Parameters"}, {}];
    If[AssociationQ[specifications], specifications = Values[specifications]];
    If[! ListQ[specifications], specifications = {}];
    path = associationLookup[operation, {"Path"}, ""];

    Do[
        If[! AssociationQ[specification], Continue[]];
        name = ToString @ associationLookup[specification, {"Name"}, ""];
        location = canonicalName @ associationLookup[specification, {"In", "Location"}, "query"];
        required = TrueQ @ associationLookup[specification, {"Required"}, False];
        value = parameterValue[supplied, name];
        missing = MissingQ[value] || value === Null;
        If[missing && required,
            Return @ Failure[
                "MissingRequiredParameter",
                <|
                    "MessageTemplate" -> "A required operation parameter was not supplied.",
                    "Parameter" -> name
                |>
            ]
        ];
        If[missing, Continue[]];
        Switch[location,
            "path",
                serializedValue = serializeSimpleParameter[specification, value];
                If[serializedValue === $Failed,
                    Return @ Failure["UnsupportedParameterSerialization", <|"MessageTemplate" -> "A path parameter cannot be serialized with its declared OpenAPI style.", "Parameter" -> name|>]
                ];
                path = StringReplace[path, "{" <> name <> "}" -> URLEncode[serializedValue]],
            "query",
                serializedValue = serializeQueryParameter[specification, name, value];
                If[serializedValue === $Failed,
                    Return @ Failure["UnsupportedParameterSerialization", <|"MessageTemplate" -> "A query parameter cannot be serialized with its declared OpenAPI style.", "Parameter" -> name|>]
                ];
                query = Join[query, serializedValue],
            "header",
                serializedValue = serializeSimpleParameter[specification, value];
                If[serializedValue === $Failed,
                    Return @ Failure["UnsupportedParameterSerialization", <|"MessageTemplate" -> "A header parameter cannot be serialized with its declared OpenAPI style.", "Parameter" -> name|>]
                ];
                AppendTo[headers, name -> serializedValue],
            _,
                Return @ Failure[
                    "UnsupportedParameterLocation",
                    <|
                        "MessageTemplate" -> "The generated operation declares an unsupported parameter location.",
                        "Parameter" -> name,
                        "Location" -> location
                    |>
                ]
        ],
        {specification, specifications}
    ];

    <|"Path" -> path, "Query" -> query, "Headers" -> headers|>
];

requiresAuthenticationQ[operation_Association] := Module[{authentication},
    authentication = associationLookup[operation, {"Authentication"}, True];
    If[AssociationQ[authentication],
        Return[! TrueQ[associationLookup[authentication, {"Anonymous"}, False]]]
    ];
    ! MemberQ[
        {False, None, "none", "anonymous", "false"},
        Replace[authentication, value_String :> ToLowerCase[value]]
    ]
];

authenticationSchemes[operation_Association] := Module[{authentication},
    authentication = associationLookup[operation, {"Authentication"}, <||>];
    If[AssociationQ[authentication], associationLookup[authentication, {"Schemes"}, {}], {}]
];

requestValidationFailure[operationName_String, field_String, reason_String] := Failure[
    "RequestValidationFailed",
    <|
        "MessageTemplate" -> "Invalid `Operation` request: `Field` `Reason`.",
        "MessageParameters" -> <|"Operation" -> operationName, "Field" -> field, "Reason" -> reason|>,
        "Operation" -> operationName,
        "Field" -> field,
        "Reason" -> reason
    |>
];

sanitizedParameterName[value_] := Module[{name},
    name = StringJoin @ StringCases[canonicalName[value], LetterCharacter | DigitCharacter];
    If[name === "", "unnamed", StringTake[name, UpTo[64]]]
];

jsonNumberQ[value_] := NumberQ[value] && Quiet[Check[TrueQ[Im[N[value]] == 0], False]];

validateConstraints[value_, constraints_Association, operationName_String, field_String] := Module[
    {
        minimum, maximum, exclusiveMinimum, exclusiveMaximum, multipleOf,
        minLength, maxLength, minItems, maxItems, uniqueItems,
        minProperties, maxProperties, pattern, quotient, multipleQ
    },
    minimum = associationLookup[constraints, {"minimum"}, Missing["NotAvailable"]];
    maximum = associationLookup[constraints, {"maximum"}, Missing["NotAvailable"]];
    exclusiveMinimum = associationLookup[constraints, {"exclusiveMinimum"}, Missing["NotAvailable"]];
    exclusiveMaximum = associationLookup[constraints, {"exclusiveMaximum"}, Missing["NotAvailable"]];
    multipleOf = associationLookup[constraints, {"multipleOf"}, Missing["NotAvailable"]];
    minLength = associationLookup[constraints, {"minLength"}, Missing["NotAvailable"]];
    maxLength = associationLookup[constraints, {"maxLength"}, Missing["NotAvailable"]];
    minItems = associationLookup[constraints, {"minItems"}, Missing["NotAvailable"]];
    maxItems = associationLookup[constraints, {"maxItems"}, Missing["NotAvailable"]];
    uniqueItems = associationLookup[constraints, {"uniqueItems"}, False];
    minProperties = associationLookup[constraints, {"minProperties"}, Missing["NotAvailable"]];
    maxProperties = associationLookup[constraints, {"maxProperties"}, Missing["NotAvailable"]];
    pattern = associationLookup[constraints, {"pattern"}, Missing["NotAvailable"]];
    If[NumberQ[minimum] && NumericQ[value] && value < minimum, Return[requestValidationFailure[operationName, field, "below minimum"]]];
    If[NumberQ[maximum] && NumericQ[value] && value > maximum, Return[requestValidationFailure[operationName, field, "above maximum"]]];
    If[NumberQ[exclusiveMinimum] && NumericQ[value] && value <= exclusiveMinimum,
        Return[requestValidationFailure[operationName, field, "not above exclusiveMinimum"]]
    ];
    If[NumberQ[exclusiveMaximum] && NumericQ[value] && value >= exclusiveMaximum,
        Return[requestValidationFailure[operationName, field, "not below exclusiveMaximum"]]
    ];
    If[NumberQ[multipleOf] && multipleOf > 0 && NumericQ[value],
        quotient = Quiet @ Check[N[value/multipleOf], Indeterminate];
        multipleQ = NumberQ[quotient] && TrueQ[
            Abs[quotient - Round[quotient]] <= 10^-12 Max[1., Abs[quotient]]
        ];
        If[! multipleQ,
            Return[requestValidationFailure[operationName, field, "not a multipleOf value"]]
        ]
    ];
    If[IntegerQ[minLength] && StringQ[value] && StringLength[value] < minLength, Return[requestValidationFailure[operationName, field, "shorter than minLength"]]];
    If[IntegerQ[maxLength] && StringQ[value] && StringLength[value] > maxLength, Return[requestValidationFailure[operationName, field, "longer than maxLength"]]];
    If[IntegerQ[minItems] && ListQ[value] && Length[value] < minItems, Return[requestValidationFailure[operationName, field, "fewer than minItems"]]];
    If[IntegerQ[maxItems] && ListQ[value] && Length[value] > maxItems, Return[requestValidationFailure[operationName, field, "more than maxItems"]]];
    If[
        TrueQ[uniqueItems] && ListQ[value] && ! DuplicateFreeQ[
            value,
            SameQ[#1, #2] || (NumberQ[#1] && NumberQ[#2] && TrueQ[#1 == #2]) &
        ],
        Return[requestValidationFailure[operationName, field, "items are not unique"]]
    ];
    If[IntegerQ[minProperties] && AssociationQ[value] && Length[value] < minProperties,
        Return[requestValidationFailure[operationName, field, "fewer than minProperties"]]
    ];
    If[IntegerQ[maxProperties] && AssociationQ[value] && Length[value] > maxProperties,
        Return[requestValidationFailure[operationName, field, "more than maxProperties"]]
    ];
    If[StringQ[pattern] && StringQ[value] && ! Quiet[Check[StringMatchQ[value, RegularExpression[pattern]], False]],
        Return[requestValidationFailure[operationName, field, "does not match pattern"]]
    ];
    True
];

validateSchemaValue[value_, schema_Association, operationName_String, field_String, depth_Integer : 0] := Module[
    {kind, nullable, reference, resolved, enumeration, constraints, items, properties, additional, property, propertyName, propertySpec, propertyValue, known, result},
    If[depth > 64, Return[requestValidationFailure[operationName, field, "schema nesting exceeds limit"]]];
    nullable = TrueQ @ associationLookup[schema, {"Nullable"}, False];
    If[value === Null, Return[If[nullable, True, requestValidationFailure[operationName, field, "null is not allowed"]]]];
    kind = canonicalName @ associationLookup[schema, {"Kind"}, "any"];
    If[kind === "reference",
        reference = associationLookup[schema, {"Reference"}, Missing["NotAvailable"]];
        resolved = If[StringQ[reference], Lookup[$SynthyraGeneratedValidatorSpecifications, reference, Missing["NotAvailable"]], Missing["NotAvailable"]];
        If[! AssociationQ[resolved], Return[requestValidationFailure[operationName, field, "unresolved schema reference"]]];
        Return[validateSchemaValue[value, resolved, operationName, field, depth + 1]]
    ];
    enumeration = associationLookup[schema, {"Enum"}, Missing["NotAvailable"]];
    If[ListQ[enumeration] && ! MemberQ[enumeration, value], Return[requestValidationFailure[operationName, field, "not in enum"]]];
    Switch[kind,
        "string", If[! StringQ[value], Return[requestValidationFailure[operationName, field, "expected string"]]],
        "integer", If[! IntegerQ[value], Return[requestValidationFailure[operationName, field, "expected integer"]]],
        "number", If[! jsonNumberQ[value], Return[requestValidationFailure[operationName, field, "expected real number"]]],
        "boolean", If[! MemberQ[{True, False}, value], Return[requestValidationFailure[operationName, field, "expected boolean"]]],
        "array",
            If[! ListQ[value], Return[requestValidationFailure[operationName, field, "expected array"]]];
            items = associationLookup[schema, {"Items"}, Missing["NotAvailable"]];
            If[AssociationQ[items],
                Do[
                    result = validateSchemaValue[value[[index]], items, operationName, field <> "[" <> ToString[index] <> "]", depth + 1];
                    If[FailureQ[result], Return[result]],
                    {index, Length[value]}
                ]
            ],
        "object",
            If[! AssociationQ[value], Return[requestValidationFailure[operationName, field, "expected object"]]];
            If[TrueQ @ associationLookup[schema, {"Opaque"}, False], Return[True]];
            properties = associationLookup[schema, {"Properties"}, <||>];
            If[! AssociationQ[properties], Return[requestValidationFailure[operationName, field, "invalid object schema"]]];
            result = Catch[
                Do[
                    propertyName = ToString[First[property]];
                    propertySpec = Last[property];
                    propertyValue = associationLookup[value, {propertyName}, Missing["NotProvided"]];
                    If[MissingQ[propertyValue],
                        If[TrueQ @ associationLookup[propertySpec, {"Required"}, False],
                            Throw[requestValidationFailure[operationName, field <> "." <> propertyName, "required property missing"]]
                        ],
                        result = validateSchemaValue[propertyValue, associationLookup[propertySpec, {"Schema"}, <||>], operationName, field <> "." <> propertyName, depth + 1];
                        If[FailureQ[result], Throw[result]]
                    ],
                    {property, Normal[properties]}
                ]
            ];
            If[FailureQ[result], Return[result]];
            additional = associationLookup[schema, {"AdditionalProperties"}, True];
            known = canonicalName /@ Keys[properties];
            If[additional === False,
                If[AnyTrue[Keys[value], ! MemberQ[known, canonicalName[#]] &],
                    Return[requestValidationFailure[operationName, field, "additional properties are not allowed"]]
                ]
            ];
            If[AssociationQ[additional],
                result = Catch[
                    Do[
                        If[! MemberQ[known, canonicalName[First[property]]],
                            result = validateSchemaValue[
                                Last[property],
                                additional,
                                operationName,
                                field <> "." <> sanitizedParameterName[First[property]],
                                depth + 1
                            ];
                            If[FailureQ[result], Throw[result]]
                        ],
                        {property, Normal[value]}
                    ]
                ]
                ;
                If[FailureQ[result], Return[result]]
            ],
        "any" | "opaque", Null,
        _, Return[requestValidationFailure[operationName, field, "unsupported schema kind"]]
    ];
    constraints = associationLookup[schema, {"Constraints"}, <||>];
    If[AssociationQ[constraints],
        result = validateConstraints[value, constraints, operationName, field];
        If[FailureQ[result], Return[result]]
    ];
    True
];

validateOperationRequest[operationName_String, operation_Association, parts_Association] := Module[
    {
        specifications, declaredNames, suppliedKeys, undeclaredKey,
        name, required, value, schema, result, bodySpec, body
    },
    specifications = associationLookup[operation, {"Parameters"}, {}];
    If[AssociationQ[specifications], specifications = Values[specifications]];
    If[! ListQ[specifications], Return[requestValidationFailure[operationName, "Parameters", "invalid generated parameter metadata"]]];
    If[! AllTrue[specifications, AssociationQ],
        Return[requestValidationFailure[operationName, "Parameters", "invalid generated parameter metadata"]]
    ];
    declaredNames = canonicalName /@ (
        ToString @ associationLookup[#, {"Name"}, ""] & /@ specifications
    );
    suppliedKeys = Keys[parts["Parameters"]];
    undeclaredKey = SelectFirst[
        suppliedKeys,
        ! MemberQ[declaredNames, canonicalName[#]] &,
        Missing["NotFound"]
    ];
    If[! MissingQ[undeclaredKey],
        Return @ requestValidationFailure[
            operationName,
            "Parameters." <> sanitizedParameterName[undeclaredKey],
            "parameter is not declared by the operation contract"
        ]
    ];
    result = Catch[
        Do[
            name = ToString @ associationLookup[specification, {"Name"}, ""];
            required = TrueQ @ associationLookup[specification, {"Required"}, False];
            value = parameterValue[parts["Parameters"], name];
            If[MissingQ[value] || value === Null,
                If[required, Throw[requestValidationFailure[operationName, "Parameters." <> name, "required parameter missing"]]],
                schema = associationLookup[specification, {"Schema"}, <||>];
                result = validateSchemaValue[value, schema, operationName, "Parameters." <> name];
                If[FailureQ[result], Throw[result]]
            ],
            {specification, specifications}
        ]
    ];
    If[FailureQ[result], Return[result]];
    bodySpec = associationLookup[operation, {"RequestBody"}, Missing["NotAvailable"]];
    body = parts["Body"];
    If[AssociationQ[bodySpec],
        If[MissingQ[body],
            If[TrueQ @ associationLookup[bodySpec, {"Required"}, False], Return[requestValidationFailure[operationName, "Body", "required body missing"]]],
            schema = associationLookup[bodySpec, {"Schema"}, <||>];
            result = validateSchemaValue[body, schema, operationName, "Body"];
            If[FailureQ[result], Return[result]]
        ]
    ];
    True
];

(* Validation matches keys loosely ("InputsA" finds inputs_a), so the body is sent with each key
   renamed to the name the schema declares; the server matches exactly. *)
canonicalizeValue[value_, schema_Association, depth_Integer : 0] := Module[
    {kind, reference, resolved, properties, additional, declared, items},
    If[depth > 64, Return[value]];
    kind = canonicalName @ associationLookup[schema, {"Kind"}, "any"];
    If[kind === "reference",
        reference = associationLookup[schema, {"Reference"}, Missing["NotAvailable"]];
        resolved = If[StringQ[reference], Lookup[$SynthyraGeneratedValidatorSpecifications, reference, Missing["NotAvailable"]], Missing["NotAvailable"]];
        Return[If[AssociationQ[resolved], canonicalizeValue[value, resolved, depth + 1], value]]
    ];
    Which[
        kind === "array" && ListQ[value],
            items = associationLookup[schema, {"Items"}, Missing["NotAvailable"]];
            If[AssociationQ[items], canonicalizeValue[#, items, depth + 1] & /@ value, value],
        kind === "object" && AssociationQ[value] && ! TrueQ[associationLookup[schema, {"Opaque"}, False]],
            properties = associationLookup[schema, {"Properties"}, <||>];
            If[! AssociationQ[properties], properties = <||>];
            additional = associationLookup[schema, {"AdditionalProperties"}, True];
            declared = AssociationThread[canonicalName /@ Keys[properties], Keys[properties]];
            Association @ KeyValueMap[
                Function[{key, entry},
                    With[{name = Lookup[declared, canonicalName[key], Missing["Undeclared"]]},
                        If[MissingQ[name],
                            key -> If[AssociationQ[additional], canonicalizeValue[entry, additional, depth + 1], entry],
                            name -> canonicalizeValue[entry, associationLookup[properties[name], {"Schema"}, <||>], depth + 1]
                        ]
                    ]
                ],
                value
            ],
        True, value
    ]
];

acceptHeaderFor[operation_Association] := Switch[
    canonicalName @ associationLookup[operation, {"ResultKind"}, "json"],
    "jsonl", "application/x-ndjson, application/jsonl, application/json",
    "binary" | "download" | "bytes" | "pdf" | "zip" | "file", "application/octet-stream",
    _, "application/json"
];

buildHTTPRequest[client_SynthyraClientObject, operationName_String, operation_Association, request_] := Module[
    {parts, validation, serialized, credential, method, url, headers, body, requestData, schemes},
    parts = requestParts[operation, request];
    If[FailureQ[parts], Return[parts]];
    validation = validateOperationRequest[operationName, operation, parts];
    If[FailureQ[validation], Return[validation]];
    serialized = serializeParameters[operation, parts["Parameters"]];
    If[FailureQ[serialized], Return[serialized]];

    credential = credentialForClient[client];
    If[FailureQ[credential], Return[credential]];
    If[requiresAuthenticationQ[operation] && credential === None,
        Return[missingCredentialFailure[]]
    ];

    method = ToUpperCase @ ToString @ associationLookup[operation, {"Method"}, "GET"];
    url = client[[1, "BaseURL"]] <> serialized["Path"];
    If[serialized["Query"] =!= {}, url = URLBuild[url, serialized["Query"]]];

    headers = Join[
        {
            "Accept" -> acceptHeaderFor[operation],
            "User-Agent" -> ("SynthyraLink/" <> $SynthyraPacletVersion)
        },
        serialized["Headers"]
    ];
    schemes = authenticationSchemes[operation];
    If[validCredentialString[credential] && (requiresAuthenticationQ[operation] || Length[schemes] > 0),
        AppendTo[headers, "Authorization" -> ("Bearer " <> credential)]
    ];

    requestData = <|Method -> method, "Headers" -> headers|>;
    body = parts["Body"];
    With[{bodySpec = associationLookup[operation, {"RequestBody"}, Missing["NotAvailable"]]},
        If[! MissingQ[body] && AssociationQ[bodySpec],
            body = canonicalizeValue[body, associationLookup[bodySpec, {"Schema"}, <||>]]
        ]
    ];
    If[! MissingQ[body],
        AssociateTo[requestData, <|
            "Body" -> StringTrim @ ExportString[body, "RawJSON"],
            "ContentType" -> "application/json"
        |>]
    ];
    HTTPRequest[url, requestData]
];

$SynthyraHTTPTransport = Function[{request, timeout},
    URLRead[
        request,
        {"StatusCode", "Headers", "ContentType", "Body", "BodyByteArray"},
        VerifySecurityCertificates -> True,
        TimeConstraint -> timeout
    ]
];
$SynthyraDownloadTransport = Function[{request, path, timeout},
    URLDownload[
        request,
        path,
        {"File", "StatusCode", "Headers", "ContentType"},
        VerifySecurityCertificates -> True,
        TimeConstraint -> timeout
    ]
];
$SynthyraPauseFunction = Pause;
$SynthyraClockFunction = AbsoluteTime;

responseHeaders[response_Association] := Module[{headers = Lookup[response, "Headers", {}]},
    Which[
        AssociationQ[headers], Normal[headers],
        MatchQ[headers, {___Rule}], headers,
        True, {}
    ]
];

headerValue[headers_, name_String, default_: Missing["NotAvailable"]] := Module[{match},
    match = SelectFirst[
        headers,
        canonicalName[First[#]] === canonicalName[name] &,
        Missing["NotFound"]
    ];
    If[MissingQ[match], default, Last[match]]
];

sensitiveHeaderQ[name_] := MemberQ[
    {"authorization", "proxyauthorization", "cookie", "setcookie"},
    canonicalName[name]
];

sensitiveValueKeyQ[name_] := sensitiveHeaderQ[name] || MemberQ[
    {
        "apikey", "token", "password", "secret",
        "outputurl", "resulturl", "downloadurl", "artifacturl",
        "signedurl", "presignedurl", "signedartifacturl"
    },
    canonicalName[name]
];

sanitizeHeaders[headers_] := Select[responseHeaders[<|"Headers" -> headers|>], ! sensitiveHeaderQ[First[#]] &];

redactString[value_String, credential_] := Module[{rules},
    rules = {RegularExpression["(?i)bearer\\s+[^\\s\\\",;]+"] -> "Bearer <redacted>"};
    If[validCredentialString[credential], PrependTo[rules, credential -> "<redacted>"]];
    StringReplace[value, rules]
];

sanitizeValue[value_, credential_] := Which[
    StringQ[value], redactString[value, credential],
    FailureQ[value], Failure[First[value], sanitizeValue[value[[2]], credential]],
    AssociationQ[value], Association @ KeyValueMap[
        If[sensitiveValueKeyQ[#1],
            #1 -> "<redacted>",
            #1 -> sanitizeValue[#2, credential]
        ] &,
        value
    ],
    ListQ[value], sanitizeValue[#, credential] & /@ value,
    MatchQ[value, _Rule], If[
        sensitiveValueKeyQ[First[value]],
        First[value] -> "<redacted>",
        First[value] -> sanitizeValue[Last[value], credential]
    ],
    True, value
];

requestIdentifier[response_Association] := Module[{headers = responseHeaders[response]},
    headerValue[
        headers,
        "x-synthyra-request-id",
        headerValue[headers, "x-request-id", headerValue[headers, "traceparent", Missing["NotAvailable"]]]
    ]
];

safeReadQ[operation_Association] := MemberQ[
    {"GET", "HEAD", "OPTIONS"},
    ToUpperCase @ ToString @ associationLookup[operation, {"Method"}, "GET"]
] && associationLookup[operation, {"SafeToRetry"}, True] =!= False;

transientStatusQ[status_] := MemberQ[{429, 502, 503, 504}, status];

(* The Gateway answers 503 with one of these error codes before the request reaches a model or
   billing, so even a POST is safe to send again: warming_up after its 85 s wait for a cold
   GPU container, auth_unavailable when the key service could not be reached. *)
$SynthyraNotExecutedErrors = {"warming_up", "auth_unavailable"};

responseErrorCode[response_Association] := Module[{parsed},
    parsed = Quiet @ Check[ImportString[responseBodyString[response], "RawJSON"], $Failed];
    If[AssociationQ[parsed],
        ToString @ associationLookup[parsed, {"error"}, associationLookup[
            Replace[associationLookup[parsed, {"detail"}, <||>], Except[_Association] -> <||>],
            {"error"},
            ""
        ]],
        ""
    ]
];

notExecutedQ[response_Association] :=
    Lookup[response, "StatusCode", Missing["NotAvailable"]] === 503 &&
    MemberQ[$SynthyraNotExecutedErrors, responseErrorCode[response]];

(* Shown while a request waits to be retried; a notebook sees why the call is slow. *)
$SynthyraRetryNotifier = Function[{operationName, reason, delay},
    If[TrueQ[$Notebooks],
        PrintTemporary[Row[{
            "Synthyra ", operationName, ": ", reason, ", retrying in ", Round[delay], " s"
        }]]
    ]
];

retryDelay[response_Association, retryNumber_Integer, policy_] := Module[{retryAfter, parsed},
    retryAfter = headerValue[responseHeaders[response], "retry-after", Missing["NotAvailable"]];
    parsed = With[{text = StringTrim[ToString[retryAfter]]},
        If[StringLength[text] <= 10 && StringMatchQ[text, DigitCharacter ..], FromDigits[text], $Failed]
    ];
    If[NumberQ[parsed] && parsed >= 0, Return[Min[N[parsed], 60.]]];
    Which[
        NumberQ[policy], N[policy] * 2.^(retryNumber - 1),
        ListQ[policy] && Length[policy] > 0, N @ policy[[Min[retryNumber, Length[policy]]]],
        Head[policy] === Function, N @ policy[retryNumber],
        True, 0.
    ]
];

performRequest[request_HTTPRequest, operation_Association, operationName_String, maxRetries_Integer, backoff_, timeout_] := Module[
    {response, retryNumber = 0, delay, notExecuted},
    While[True,
        response = Quiet @ Check[$SynthyraHTTPTransport[request, timeout], $Failed];
        (* A replay names the request its recording lacks. Other transport failures can carry the
           request and its credential, so they are replaced, never passed on. *)
        If[MatchQ[response, Failure["NotRecorded", _]], Return[response]];
        If[response === $Failed || FailureQ[response] || ! AssociationQ[response],
            Return @ Failure[
                "HTTPTransportError",
                <|
                    "MessageTemplate" -> "The HTTPS request for `Operation` could not be completed.",
                    "MessageParameters" -> <|"Operation" -> operationName|>,
                    "Operation" -> operationName
                |>
            ]
        ];
        notExecuted = notExecutedQ[response];
        If[
            (notExecuted || (safeReadQ[operation] && transientStatusQ[Lookup[response, "StatusCode", Missing["NotAvailable"]]])) &&
            retryNumber < maxRetries,
            retryNumber++;
            delay = retryDelay[response, retryNumber, backoff];
            If[notExecuted, $SynthyraRetryNotifier[operationName, StringReplace[responseErrorCode[response], "_" -> " "], delay]];
            $SynthyraPauseFunction[delay];
            Continue[]
        ];
        Return[response]
    ]
];

downloadAttemptPath[path_String] := Module[{absolute, directory, baseName},
    absolute = ExpandFileName[path];
    directory = DirectoryName[absolute];
    baseName = FileNameTake[absolute];
    FileNameJoin[{
        directory,
        "." <> baseName <> ".synthyralink-" <> CreateUUID[] <> ".part"
    }]
];

downloadedFilePath[response_Association, fallback_String] := Module[{file},
    file = Lookup[response, "File", File[fallback]];
    Which[
        MatchQ[file, File[_String]], First[file],
        StringQ[file], file,
        True, fallback
    ]
];

sameFilePathQ[first_String, second_String] := Quiet @ Check[
    ExpandFileName[first] === ExpandFileName[second],
    first === second
];

deleteDownloadFile[path_, protected_] := If[
    StringQ[path] && FileExistsQ[path] &&
        ! (StringQ[protected] && sameFilePathQ[path, protected]),
    Quiet[Check[DeleteFile[path], Null]]
];

cleanupDownloadFiles[paths_List, protected_: None] := Scan[
    deleteDownloadFile[#, protected] &,
    DeleteDuplicates @ Select[paths, StringQ]
];

readDownloadedBytes[path_String] := Module[{stream, values},
    Quiet @ Check[
        stream = OpenRead[path, BinaryFormat -> True];
        values = BinaryReadList[stream, "UnsignedInteger8"];
        Close[stream];
        ByteArray[values],
        If[Head[stream] === InputStream, Quiet[Close[stream]]];
        $Failed
    ]
];

attachDownloadedErrorBody[response_Association, path_String] := Module[
    {result = response, bytes, body},
    bytes = readDownloadedBytes[path];
    If[Head[bytes] === ByteArray,
        body = Quiet[Check[ByteArrayToString[bytes, "UTF8"], ""]];
        AssociateTo[result, <|"BodyByteArray" -> bytes, "Body" -> body|>]
    ];
    result
];

finalizeDownloadedFile[downloadedPath_String, targetPath_String, operationName_String] := Module[
    {absoluteTarget, moved},
    absoluteTarget = ExpandFileName[targetPath];
    If[! FileExistsQ[downloadedPath],
        Return @ Failure[
            "DownloadedFileMissing",
            <|
                "MessageTemplate" -> "The HTTPS download completed without producing a local file.",
                "Operation" -> operationName,
                "Target" -> File[targetPath]
            |>
        ]
    ];
    If[sameFilePathQ[downloadedPath, absoluteTarget], Return[absoluteTarget]];
    moved = Quiet @ Check[
        RenameFile[downloadedPath, absoluteTarget, OverwriteTarget -> True],
        $Failed
    ];
    If[moved === $Failed || ! FileExistsQ[absoluteTarget],
        Failure[
            "DownloadWriteFailed",
            <|
                "MessageTemplate" -> "The downloaded response could not be committed to the requested target.",
                "Operation" -> operationName,
                "Target" -> File[targetPath]
            |>
        ],
        absoluteTarget
    ]
];

performDownloadRequest[request_HTTPRequest, path_String, operation_Association, operationName_String, maxRetries_Integer, backoff_, timeout_] := Module[
    {response, retryNumber = 0, attemptPath, downloadedPath, status, finalized},
    While[True,
        attemptPath = downloadAttemptPath[path];
        response = Quiet @ Check[$SynthyraDownloadTransport[request, attemptPath, timeout], $Failed];
        If[response === $Failed || FailureQ[response] || ! AssociationQ[response],
            cleanupDownloadFiles[{attemptPath}, path];
            Return @ Failure[
                "DownloadTransportError",
                <|
                    "MessageTemplate" -> "The HTTPS download could not be completed.",
                    "Operation" -> operationName
                |>
            ]
        ];
        downloadedPath = downloadedFilePath[response, attemptPath];
        status = Lookup[response, "StatusCode", Missing["NotAvailable"]];
        If[
            safeReadQ[operation] &&
            transientStatusQ[status] &&
            retryNumber < maxRetries,
            cleanupDownloadFiles[{attemptPath, downloadedPath}, path];
            retryNumber++;
            $SynthyraPauseFunction[retryDelay[response, retryNumber, backoff]];
            Continue[]
        ];
        If[IntegerQ[status] && 200 <= status < 300,
            finalized = finalizeDownloadedFile[downloadedPath, path, operationName];
            cleanupDownloadFiles[{attemptPath, downloadedPath}, path];
            If[FailureQ[finalized], Return[finalized]];
            AssociateTo[response, "File" -> File[path]];
            Return[response]
        ];
        response = attachDownloadedErrorBody[response, downloadedPath];
        cleanupDownloadFiles[{attemptPath, downloadedPath}, path];
        Return[response]
    ]
];

responseBodyString[response_Association] := Module[{body = Lookup[response, "Body", ""]},
    If[StringQ[body], body, ToString[body, InputForm]]
];

responseBodyBytes[response_Association] := Module[{bytes = Lookup[response, "BodyByteArray", Missing["NotAvailable"]]},
    Which[
        Head[bytes] === ByteArray, bytes,
        ListQ[bytes] && VectorQ[bytes, IntegerQ], ByteArray[bytes],
        True, StringToByteArray[responseBodyString[response], "UTF8"]
    ]
];

responseContentType[response_Association] := Module[{contentType},
    contentType = Lookup[response, "ContentType", Missing["NotAvailable"]];
    If[MissingQ[contentType], contentType = headerValue[responseHeaders[response], "content-type", ""]];
    ToLowerCase @ First @ StringSplit[ToString[contentType], ";"]
];

parseJSON[text_String, operationName_String] := Module[{parsed},
    If[StringLength[StringTrim[text]] === 0, Return[Null]];
    parsed = Quiet[Check[ImportString[text, "RawJSON"], $Failed]];
    If[parsed === $Failed,
        Failure[
            "InvalidJSONResponse",
            <|
                "MessageTemplate" -> "The server returned a response that is not valid JSON.",
                "Operation" -> operationName
            |>
        ],
        parsed
    ]
];

parseJSONL[text_String, operationName_String] := Module[{lines, parsed},
    lines = Select[StringSplit[text, {"\r\n", "\n", "\r"}], StringLength[StringTrim[#]] > 0 &];
    parsed = Quiet[Check[ImportString[#, "RawJSON"] & /@ lines, $Failed]];
    If[parsed === $Failed,
        Failure[
            "InvalidJSONLResponse",
            <|
                "MessageTemplate" -> "The server returned an invalid JSON Lines record.",
                "Operation" -> operationName
            |>
        ],
        parsed
    ]
];

writeBinaryTarget[bytes_ByteArray, Automatic] := bytes;
writeBinaryTarget[bytes_ByteArray, File[path_String]] := Module[{stream, result},
    result = Quiet @ Check[
        stream = OpenWrite[path, BinaryFormat -> True];
        BinaryWrite[stream, Normal[bytes], "UnsignedInteger8"];
        Close[stream];
        File[path],
        If[Head[stream] === OutputStream, Quiet[Close[stream]]];
        $Failed
    ];
    If[result === $Failed,
        Failure[
            "DownloadWriteFailed",
            <|
                "MessageTemplate" -> "The binary response could not be written to the requested target.",
                "Target" -> File[path]
            |>
        ],
        result
    ]
];
writeBinaryTarget[_, target_] := Failure[
    "InvalidDownloadTarget",
    <|
        "MessageTemplate" -> "Target must be Automatic or File[path].",
        "Target" -> target
    |>
];

parseSuccessfulResponse[response_Association, operation_Association, operationName_String, returnType_, target_] := Module[
    {kind, contentType, parsed},
    kind = canonicalName @ associationLookup[operation, {"ResultKind"}, "json"];
    contentType = responseContentType[response];
    parsed = Switch[kind,
        "jsonl" | "ndjson",
            parseJSONL[responseBodyString[response], operationName],
        "binary" | "download" | "bytes" | "pdf" | "zip" | "file",
            writeBinaryTarget[responseBodyBytes[response], target],
        "scorematrix" | "matrix" | "uint8matrix",
            With[{json = parseJSON[responseBodyString[response], operationName]},
                If[FailureQ[json], json, SynthyraScoreMatrix[json]]
            ],
        _,
            If[
                MemberQ[{"application/octet-stream", "application/zip", "application/pdf"}, contentType],
                writeBinaryTarget[responseBodyBytes[response], target],
                parseJSON[responseBodyString[response], operationName]
            ]
    ];
    If[FailureQ[parsed], Return[parsed]];
    If[returnType === "Dataset" && ListQ[parsed], Dataset[parsed], parsed]
];

sanitizedRawResponse[response_Association, credential_] := <|
    "StatusCode" -> Lookup[response, "StatusCode", Missing["NotAvailable"]],
    "Headers" -> sanitizeValue[Association @ sanitizeHeaders[responseHeaders[response]], credential],
    "Body" -> sanitizeValue[
        If[
            MemberQ[
                {
                    "application/octet-stream", "application/zip", "application/pdf",
                    "application/x-gzip", "application/gzip"
                },
                responseContentType[response]
            ] || StringStartsQ[responseContentType[response], "image/"],
            responseBodyBytes[response],
            responseBodyString[response]
        ],
        credential
    ]
|>;

(* The sentence a server error body carries: Gateway errors put it under "message" or "detail". *)
serverMessage[parsed_] := Module[{text},
    text = Which[
        StringQ[parsed], parsed,
        AssociationQ[parsed], Replace[
            associationLookup[parsed, {"message", "detail", "error"}, ""],
            {
                detail_Association :> associationLookup[detail, {"message", "detail", "error"}, ""],
                detail_List :> StringRiffle[ToString /@ Flatten[{detail}], "; "]
            }
        ],
        True, ""
    ];
    text = StringTrim @ ToString[text];
    If[text === "", "no reason given", StringTake[text, UpTo[300]]]
];

httpFailure[response_Association, operationName_String, credential_] := Module[
    {body, parsed, requestID, status, reason},
    body = responseBodyString[response];
    parsed = Quiet[Check[ImportString[body, "RawJSON"], body]];
    requestID = requestIdentifier[response];
    status = Lookup[response, "StatusCode", Missing["NotAvailable"]];
    reason = sanitizeValue[serverMessage[parsed], credential];
    Failure[
        "SynthyraHTTPError",
        <|
            "MessageTemplate" -> "Synthyra `Operation` failed with HTTP `StatusCode`: `Reason`",
            "MessageParameters" -> <|"Operation" -> operationName, "StatusCode" -> status, "Reason" -> reason|>,
            "Operation" -> operationName,
            "StatusCode" -> status,
            "Reason" -> reason,
            "RequestID" -> requestID,
            "ServerResponse" -> sanitizeValue[parsed, credential]
        |>
    ]
];

jobSpecification[operation_Association] := associationLookup[operation, {"Job", "Async"}, Missing["NotAvailable"]];

streamingDownloadQ[operation_Association, target_] := MatchQ[target, File[_String]] && MemberQ[
    {"binary", "download", "bytes", "pdf", "zip", "file"},
    canonicalName @ associationLookup[operation, {"ResultKind"}, "json"]
];

sanitizedDownloadResponse[response_Association, credential_, path_String] := <|
    "StatusCode" -> Lookup[response, "StatusCode", Missing["NotAvailable"]],
    "Headers" -> sanitizeValue[Association @ sanitizeHeaders[responseHeaders[response]], credential],
    "Body" -> File[path]
|>;

extractJobIdentifier[result_] := Module[{candidate},
    If[! AssociationQ[result], Return[Missing["NotAvailable"]]];
    candidate = associationLookup[result, {"job_id", "jobId", "id", "task_id", "taskId"}, Missing["NotAvailable"]];
    If[StringQ[candidate] || IntegerQ[candidate], ToString[candidate], Missing["NotAvailable"]]
];

makeJobObject[client_, operationName_String, operation_Association, result_] := Module[
    {specification, jobID},
    specification = jobSpecification[operation];
    If[MissingQ[specification] || specification === False, Return[result]];
    If[specification === True, specification = <||>];
    If[! AssociationQ[specification],
        Return @ Failure[
            "InvalidJobMetadata",
            <|
                "MessageTemplate" -> "The generated asynchronous-operation metadata is invalid.",
                "Operation" -> operationName
            |>
        ]
    ];
    jobID = extractJobIdentifier[result];
    If[MissingQ[jobID],
        Return @ Failure[
            "MissingJobIdentifier",
            <|
                "MessageTemplate" -> "The asynchronous submission response did not contain a job identifier.",
                "Operation" -> operationName
            |>
        ]
    ];
    SynthyraJobObject[
        <|
            "Client" -> client,
            "SubmissionOperation" -> operationName,
            "JobID" -> jobID,
            "Job" -> specification,
            "Submission" -> result
        |>
    ]
];

Options[SynthyraExecute] = {
    ReturnType -> "Association",
    Target -> Automatic,
    RawResponse -> False,
    MaxRetries -> 3,
    RetryBackoff -> {0.5, 1., 2.},
    Timeout -> 120.
};

SynthyraExecute[client_SynthyraClientObject, name_String] := SynthyraExecute[client, name, <||>];

SynthyraExecute[
    client_SynthyraClientObject,
    name_String,
    firstOption : (_Rule | _RuleDelayed),
    opts : OptionsPattern[]
] := SynthyraExecute[client, name, <||>, firstOption, opts];

SynthyraExecute[client_SynthyraClientObject, name_String, request_?nonOptionRequestQ, OptionsPattern[]] := Module[
    {operationName, operation, httpRequest, response, status, credential, parsed, downloadPath, maxRetries, timeout, returnType, target},
    maxRetries = OptionValue[MaxRetries];
    timeout = OptionValue[Timeout];
    returnType = OptionValue[ReturnType];
    target = OptionValue[Target];
    If[! IntegerQ[maxRetries] || maxRetries < 0,
        Return @ Failure["InvalidOption", <|"MessageTemplate" -> "MaxRetries must be a nonnegative integer.", "Option" -> MaxRetries|>]
    ];
    If[! NumericQ[timeout] || timeout <= 0,
        Return @ Failure["InvalidOption", <|"MessageTemplate" -> "Timeout must be a positive number.", "Option" -> Timeout|>]
    ];
    If[! MemberQ[{"Association", "Dataset"}, returnType],
        Return @ Failure["InvalidOption", <|"MessageTemplate" -> "ReturnType must be \"Association\" or \"Dataset\".", "Option" -> ReturnType|>]
    ];
    If[! (target === Automatic || MatchQ[target, File[_String]]),
        Return @ Failure["InvalidOption", <|"MessageTemplate" -> "Target must be Automatic or File[path].", "Option" -> Target|>]
    ];
    operationName = resolveOperationName[name];
    operation = operationInformation[operationName];
    If[FailureQ[operation], Return[operation]];
    If[MatchQ[target, File[_String]] && ! MemberQ[
        {"binary", "download", "bytes", "pdf", "zip", "file"},
        canonicalName @ associationLookup[operation, {"ResultKind"}, "json"]
    ],
        Return @ Failure[
            "InvalidDownloadTarget",
            <|"MessageTemplate" -> "Target -> File[path] is supported only for binary download operations.", "Operation" -> operationName|>
        ]
    ];

    httpRequest = buildHTTPRequest[client, operationName, operation, request];
    If[FailureQ[httpRequest], Return[httpRequest]];

    If[streamingDownloadQ[operation, target],
        downloadPath = First[target];
        response = performDownloadRequest[
            httpRequest,
            downloadPath,
            operation,
            operationName,
            maxRetries,
            OptionValue[RetryBackoff],
            timeout
        ];
        If[FailureQ[response], Return[response]];
        credential = credentialForClient[client];
        If[FailureQ[credential], credential = None];
        status = Lookup[response, "StatusCode", Missing["NotAvailable"]];
        If[TrueQ[OptionValue[RawResponse]],
            Return @ If[
                IntegerQ[status] && 200 <= status < 300,
                sanitizedDownloadResponse[response, credential, downloadPath],
                sanitizedRawResponse[response, credential]
            ]
        ];
        If[! IntegerQ[status] || status < 200 || status >= 300,
            Return[httpFailure[response, operationName, credential]]
        ];
        Return[File[downloadPath]]
    ];

    response = performRequest[
        httpRequest,
        operation,
        operationName,
        maxRetries,
        OptionValue[RetryBackoff],
        timeout
    ];
    If[FailureQ[response], Return[response]];
    credential = credentialForClient[client];
    If[FailureQ[credential], credential = None];

    If[TrueQ[OptionValue[RawResponse]],
        Return[sanitizedRawResponse[response, credential]]
    ];

    status = Lookup[response, "StatusCode", Missing["NotAvailable"]];
    If[! IntegerQ[status] || status < 200 || status >= 300,
        Return[httpFailure[response, operationName, credential]]
    ];

    parsed = parseSuccessfulResponse[
        response,
        operation,
        operationName,
        returnType,
        target
    ];
    If[FailureQ[parsed], Return[parsed]];
    makeJobObject[client, operationName, operation, parsed]
];

SynthyraExecute[___] := Failure[
    "InvalidExecuteArguments",
    <|"MessageTemplate" -> "SynthyraExecute requires a SynthyraClientObject, an operation name, and an optional request."|>
];

SynthyraJobObject /: MakeBoxes[job : SynthyraJobObject[data_Association], form_] := InterpretationBox[
    RowBox[{
        "SynthyraJobObject", "[",
        ToBoxes[
            <|
                "Operation" -> Lookup[data, "SubmissionOperation", Missing["NotAvailable"]],
                "JobID" -> Lookup[data, "JobID", Missing["NotAvailable"]],
                "Status" -> associationLookup[Lookup[data, "LastStatus", <||>], {"status", "state"}, "Submitted"]
            |>,
            form
        ],
        "]"
    }],
    SynthyraJobObject[KeyDrop[data, {"LastStatus", "Submission"}]]
];

SynthyraJobObject /: Normal[SynthyraJobObject[data_Association]] :=
    sanitizeValue[KeyDrop[data, {"Client", "Submission"}], None];

jobMetadataValue[job_Association, names_List, default_: Missing["NotAvailable"]] :=
    associationLookup[Lookup[job, "Job", <||>], names, default];

jobOperationName[job_Association, names_List] := Module[{name},
    name = jobMetadataValue[job, names, Missing["NotAvailable"]];
    If[StringQ[name], resolveOperationName[name], Missing["NotAvailable"]]
];

jobIdentifierParameter[operationName_] := Module[{operation, specifications, pathParameters, names},
    If[! StringQ[operationName], Return[Missing["NotAvailable"]]];
    operation = operationInformation[operationName];
    If[FailureQ[operation], Return[Missing["NotAvailable"]]];
    specifications = associationLookup[operation, {"Parameters"}, {}];
    If[AssociationQ[specifications], specifications = Values[specifications]];
    If[! ListQ[specifications], Return[Missing["NotAvailable"]]];
    pathParameters = Select[
        specifications,
        AssociationQ[#] && canonicalName[associationLookup[#, {"In", "Location"}, ""]] === "path" &
    ];
    names = ToString[associationLookup[#, {"Name"}, ""]] & /@ pathParameters;
    SelectFirst[names, StringContainsQ[canonicalName[#], "jobid"] &, FirstCase[names, _String?(StringLength[#] > 0 &), Missing["NotAvailable"]]]
];

jobRequest[job_Association, operationName_: Missing["NotAvailable"]] := Module[{parameter},
    parameter = jobMetadataValue[job, {"IdentifierParameter", "JobIdParameter", "JobIDParameter"}, "job_id"];
    If[
        parameter === "job_id" && StringQ[operationName],
        parameter = Replace[jobIdentifierParameter[operationName], _Missing -> parameter]
    ];
    <|"Parameters" -> <|ToString[parameter] -> Lookup[job, "JobID"]|>|>
];

Options[SynthyraJobStatus] = Options[SynthyraExecute];

rawJobStatus[data_Association, opts___] := Module[{operation},
    operation = jobOperationName[data, {"StatusOperation", "StatusOperationId", "statusOperationId"}];
    If[MissingQ[operation],
        Return @ Failure[
            "MissingStatusOperation",
            <|
                "MessageTemplate" -> "The asynchronous operation does not declare a status operation.",
                "Operation" -> Lookup[data, "SubmissionOperation", Missing["NotAvailable"]]
            |>
        ]
    ];
    SynthyraExecute[
        Lookup[data, "Client"],
        operation,
        jobRequest[data, operation],
        Sequence @@ FilterRules[{opts}, Options[SynthyraExecute]]
    ]
];

SynthyraJobStatus[SynthyraJobObject[data_Association], opts : OptionsPattern[]] := sanitizedJobStatus[
    data,
    rawJobStatus[data, Sequence @@ FilterRules[{opts}, Options[SynthyraExecute]]]
];

jobState[status_] := If[
    AssociationQ[status],
    ToLowerCase @ ToString @ associationLookup[status, {"status", "state", "job_status", "jobState"}, "unknown"],
    "unknown"
];

stateList[value_, default_List] := Which[
    ListQ[value], ToLowerCase[ToString[#]] & /@ value,
    StringQ[value], {ToLowerCase[value]},
    True, default
];

terminalStateClass[job_Association, status_] := Module[
    {state, terminal, successful, failed, cancelled},
    state = jobState[status];
    terminal = jobMetadataValue[job, {"TerminalStates", "terminalStates"}, <||>];
    If[AssociationQ[terminal],
        successful = stateList[associationLookup[terminal, {"Succeeded", "Success", "Completed"}], {"completed", "succeeded", "success", "done"}];
        failed = stateList[associationLookup[terminal, {"Failed", "Failure", "Error"}], {"failed", "error", "expired"}];
        cancelled = stateList[associationLookup[terminal, {"Cancelled", "Canceled"}], {"cancelled", "canceled"}],
        successful = stateList[
            jobMetadataValue[job, {"SuccessStates", "SucceededStates", "successStates"}, Missing["NotAvailable"]],
            {"completed", "succeeded", "success", "done"}
        ];
        failed = stateList[
            jobMetadataValue[job, {"FailureStates", "FailedStates", "failureStates"}, Missing["NotAvailable"]],
            {"failed", "error", "expired"}
        ];
        cancelled = stateList[
            jobMetadataValue[job, {"CancellationStates", "CancelledStates", "cancellationStates"}, Missing["NotAvailable"]],
            {"cancelled", "canceled"}
        ];
        failed = Complement[failed, cancelled]
    ];
    Which[
        MemberQ[successful, state], "Succeeded",
        MemberQ[cancelled, state], "Cancelled",
        MemberQ[failed, state], "Failed",
        ListQ[terminal] && MemberQ[stateList[terminal, {}], state], "Failed",
        True, "Running"
    ]
];

sanitizedJobStatus[job_Association, status_] := Module[{credential},
    credential = credentialForClient[Lookup[job, "Client"]];
    If[FailureQ[credential], credential = None];
    sanitizeValue[status, credential]
];

Options[SynthyraWait] = {
    Timeout -> 3600.,
    PollInterval -> 2.
};

SynthyraWait[job_SynthyraJobObject, OptionsPattern[]] := Module[
    {data = First[job], started, status, stateClass, updated},
    If[! NumericQ[OptionValue[Timeout]] || OptionValue[Timeout] <= 0,
        Return @ Failure["InvalidOption", <|"MessageTemplate" -> "Timeout must be a positive number.", "Option" -> Timeout|>]
    ];
    If[! NumericQ[OptionValue[PollInterval]] || OptionValue[PollInterval] < 0,
        Return @ Failure["InvalidOption", <|"MessageTemplate" -> "PollInterval must be a nonnegative number.", "Option" -> PollInterval|>]
    ];
    started = $SynthyraClockFunction[];
    While[True,
        status = rawJobStatus[data];
        If[FailureQ[status], Return[status]];
        stateClass = terminalStateClass[data, status];
        updated = SynthyraJobObject[Append[data, "LastStatus" -> status]];
        Switch[stateClass,
            "Succeeded", Return[updated],
            "Failed", Return @ Failure[
                "JobFailed",
                <|
                    "MessageTemplate" -> "The Synthyra job reached a failed terminal state.",
                    "Operation" -> Lookup[data, "SubmissionOperation", Missing["NotAvailable"]],
                    "JobID" -> Lookup[data, "JobID", Missing["NotAvailable"]],
                    "Status" -> sanitizedJobStatus[data, status]
                |>
            ],
            "Cancelled", Return @ Failure[
                "JobCancelled",
                <|
                    "MessageTemplate" -> "The Synthyra job was cancelled.",
                    "Operation" -> Lookup[data, "SubmissionOperation", Missing["NotAvailable"]],
                    "JobID" -> Lookup[data, "JobID", Missing["NotAvailable"]],
                    "Status" -> sanitizedJobStatus[data, status]
                |>
            ]
        ];
        If[$SynthyraClockFunction[] - started >= OptionValue[Timeout],
            Return @ Failure[
                "JobTimeout",
                <|
                    "MessageTemplate" -> "The Synthyra job did not reach a terminal state before the timeout.",
                    "Operation" -> Lookup[data, "SubmissionOperation", Missing["NotAvailable"]],
                    "JobID" -> Lookup[data, "JobID", Missing["NotAvailable"]],
                    "LastStatus" -> sanitizedJobStatus[data, status]
                |>
            ]
        ];
        $SynthyraPauseFunction[Max[0., N @ OptionValue[PollInterval]]]
    ]
];

(* Seconds allowed for downloading a completed job's signed artifact, which can be large. *)
$SynthyraArtifactTimeout = 600.;

Options[SynthyraJobResult] = {
    WaitForCompletion -> True,
    Timeout -> 3600.,
    PollInterval -> 2.,
    ReturnType -> "Association",
    Target -> Automatic,
    RawResponse -> False,
    MaxRetries -> 3,
    RetryBackoff -> {0.5, 1., 2.}
};

signedArtifactURL[job_Association] := Module[{status, url},
    status = Lookup[job, "LastStatus", <||>];
    url = associationLookup[
        status,
        {"output_url", "outputUrl", "signed_artifact_url", "signedArtifactUrl"},
        Missing["NotAvailable"]
    ];
    If[StringQ[url] && StringLength[StringTrim[url]] > 0, StringTrim[url], Missing["NotAvailable"]]
];

absoluteHTTPSURLQ[url_String] := Module[{parsed},
    parsed = Quiet @ Check[URLParse[url], $Failed];
    AssociationQ[parsed] &&
        ToLowerCase[ToString[Lookup[parsed, "Scheme", ""]]] === "https" &&
        StringLength[ToString[Lookup[parsed, "Domain", ""]]] > 0
];
absoluteHTTPSURLQ[_] := False;

jobResultFallbackEligibleQ[failure_] := Module[{tag, data, status},
    If[! FailureQ[failure], Return[False]];
    tag = First[failure];
    data = If[AssociationQ[failure[[2]]], failure[[2]], <||>];
    status = associationLookup[data, {"StatusCode"}, Missing["NotAvailable"]];
    MemberQ[
        {"HTTPTransportError", "DownloadTransportError", "InvalidJSONResponse", "InvalidJSONLResponse"},
        tag
    ] || (tag === "SynthyraHTTPError" && MemberQ[{429, 500, 502, 503, 504}, status])
];

retrievalFailureSummary[failure_] := Module[{data, summary},
    If[! FailureQ[failure], Return[<|"Type" -> "UnknownFailure"|>]];
    data = If[AssociationQ[failure[[2]]], failure[[2]], <||>];
    summary = <|
        "Type" -> ToString[First[failure]],
        "StatusCode" -> associationLookup[data, {"StatusCode"}, Missing["NotAvailable"]],
        "RequestID" -> associationLookup[data, {"RequestID"}, Missing["NotAvailable"]]
    |>;
    Select[summary, ! MissingQ[#] &]
];

invalidSignedArtifactFailure[job_Association] := Failure[
    "InvalidSignedArtifactURL",
    <|
        "MessageTemplate" -> "The completed job exposed an invalid signed-artifact URL; only absolute HTTPS URLs are accepted.",
        "Operation" -> Lookup[job, "SubmissionOperation", Missing["NotAvailable"]],
        "JobID" -> Lookup[job, "JobID", Missing["NotAvailable"]]
    |>
];

parseSignedArtifactFile[path_String, operation_Association, operationName_String, returnType_, target_] := Module[
    {kind, parsed, text, bytes},
    kind = canonicalName @ associationLookup[operation, {"ResultKind"}, "json"];
    parsed = Switch[kind,
        "jsonl" | "ndjson",
            text = Quiet @ Check[Import[path, "Text"], $Failed];
            If[
                text === $Failed,
                Failure["InvalidJSONLResponse", <|"MessageTemplate" -> "The signed artifact could not be read as JSON Lines.", "Operation" -> operationName|>],
                parseJSONL[text, operationName]
            ],
        "binary" | "download" | "bytes" | "pdf" | "zip" | "file",
            If[
                MatchQ[target, File[_String]],
                File[ExpandFileName[First[target]]],
                bytes = readDownloadedBytes[path];
                If[
                    Head[bytes] === ByteArray,
                    bytes,
                    Failure["DownloadedFileUnreadable", <|"MessageTemplate" -> "The signed artifact could not be read.", "Operation" -> operationName|>]
                ]
            ],
        "scorematrix" | "matrix" | "uint8matrix",
            parsed = Quiet @ Check[Import[path, "RawJSON"], $Failed];
            If[
                parsed === $Failed,
                Failure["InvalidJSONResponse", <|"MessageTemplate" -> "The signed artifact is not valid JSON.", "Operation" -> operationName|>],
                SynthyraScoreMatrix[parsed]
            ],
        _,
            parsed = Quiet @ Check[Import[path, "RawJSON"], $Failed];
            If[
                parsed === $Failed,
                Failure["InvalidJSONResponse", <|"MessageTemplate" -> "The signed artifact is not valid JSON.", "Operation" -> operationName|>],
                parsed
            ]
    ];
    If[FailureQ[parsed], Return[parsed]];
    If[returnType === "Dataset" && ListQ[parsed], Dataset[parsed], parsed]
];

retrieveSignedArtifact[
    job_Association,
    artifactURL_String,
    operationName_String,
    operation_Association,
    returnType_,
    target_,
    maxRetries_Integer,
    backoff_,
    timeout_
] := Module[
    {kind, persistentTargetQ, path, request, response, status, parsed, artifactOperation},
    If[! absoluteHTTPSURLQ[artifactURL], Return[invalidSignedArtifactFailure[job]]];
    kind = canonicalName @ associationLookup[operation, {"ResultKind"}, "json"];
    persistentTargetQ = MatchQ[target, File[_String]] && MemberQ[
        {"binary", "download", "bytes", "pdf", "zip", "file"},
        kind
    ];
    path = If[
        persistentTargetQ,
        First[target],
        FileNameJoin[{$TemporaryDirectory, ".synthyralink-artifact-" <> CreateUUID[] <> ".part"}]
    ];
    artifactOperation = Join[operation, <|"Method" -> "GET", "SafeToRetry" -> True|>];
    request = HTTPRequest[
        artifactURL,
        <|
            Method -> "GET",
            "Headers" -> {
                "Accept" -> acceptHeaderFor[operation],
                "User-Agent" -> ("SynthyraLink/" <> $SynthyraPacletVersion)
            }
        |>
    ];
    response = performDownloadRequest[
        request,
        path,
        artifactOperation,
        operationName <> "SignedArtifact",
        maxRetries,
        backoff,
        timeout
    ];
    If[FailureQ[response],
        If[! persistentTargetQ, deleteDownloadFile[path, None]];
        Return[response]
    ];
    status = Lookup[response, "StatusCode", Missing["NotAvailable"]];
    If[! IntegerQ[status] || status < 200 || status >= 300,
        parsed = httpFailure[response, operationName <> "SignedArtifact", artifactURL];
        If[! persistentTargetQ, deleteDownloadFile[path, None]];
        Return[parsed]
    ];
    parsed = parseSignedArtifactFile[path, operation, operationName, returnType, target];
    If[! persistentTargetQ, deleteDownloadFile[path, None]];
    sanitizeValue[parsed, artifactURL]
];

jobResultRetrievalFailure[job_Association, primaryFailure_, artifactFailure_] := Failure[
    "JobResultRetrievalFailed",
    <|
        "MessageTemplate" -> "The completed job result could not be retrieved from either the Gateway result operation or its signed artifact.",
        "Operation" -> Lookup[job, "SubmissionOperation", Missing["NotAvailable"]],
        "JobID" -> Lookup[job, "JobID", Missing["NotAvailable"]],
        "GatewayFailure" -> retrievalFailureSummary[primaryFailure],
        "ArtifactFailure" -> retrievalFailureSummary[artifactFailure]
    |>
];

SynthyraJobResult[job_SynthyraJobObject, opts : OptionsPattern[]] := Module[
    {
        ready = job, data, operation, operationInformationData, status, submission, directResult,
        primaryResult, artifactURL, artifactResult
    },
    If[TrueQ[OptionValue[WaitForCompletion]],
        ready = SynthyraWait[
            job,
            Timeout -> OptionValue[Timeout],
            PollInterval -> OptionValue[PollInterval]
        ];
        If[FailureQ[ready], Return[ready]],
        status = rawJobStatus[First[job]];
        If[FailureQ[status], Return[status]];
        If[terminalStateClass[First[job], status] =!= "Succeeded",
            Return @ Failure[
                "JobNotComplete",
                <|"MessageTemplate" -> "The Synthyra job is not in a successful terminal state.", "Status" -> sanitizedJobStatus[First[job], status]|>
            ]
        ];
        ready = SynthyraJobObject[Append[First[job], "LastStatus" -> status]]
    ];

    data = First[ready];
    operation = jobOperationName[data, {"ResultOperation", "ResultOperationId", "resultOperationId"}];
    If[MissingQ[operation],
        submission = Lookup[data, "Submission", <||>];
        directResult = associationLookup[Lookup[data, "LastStatus", <||>], {"result", "output"}, Missing["NotAvailable"]];
        If[MissingQ[directResult], directResult = associationLookup[submission, {"result", "output"}, Missing["NotAvailable"]]];
        If[! MissingQ[directResult], Return[directResult]];
        Return @ Failure[
            "MissingResultOperation",
            <|"MessageTemplate" -> "The asynchronous operation does not declare a result operation."|>
        ]
    ];
    (* Timeout here bounds the wait for the job; the result request keeps its own HTTP timeout. *)
    primaryResult = SynthyraExecute[
        Lookup[data, "Client"],
        operation,
        jobRequest[data, operation],
        Sequence @@ FilterRules[{opts}, DeleteCases[Options[SynthyraExecute], Timeout -> _]]
    ];
    If[! FailureQ[primaryResult], Return[primaryResult]];
    artifactURL = signedArtifactURL[data];
    If[
        MissingQ[artifactURL] ||
        TrueQ[OptionValue[RawResponse]] ||
        ! jobResultFallbackEligibleQ[primaryResult],
        Return[primaryResult]
    ];
    operationInformationData = operationInformation[operation];
    If[FailureQ[operationInformationData], Return[primaryResult]];
    artifactResult = retrieveSignedArtifact[
        data,
        artifactURL,
        operation,
        operationInformationData,
        OptionValue[ReturnType],
        OptionValue[Target],
        OptionValue[MaxRetries],
        OptionValue[RetryBackoff],
        $SynthyraArtifactTimeout
    ];
    If[
        FailureQ[artifactResult],
        jobResultRetrievalFailure[data, primaryResult, artifactResult],
        artifactResult
    ]
];

Options[SynthyraCancelJob] = Options[SynthyraExecute];

SynthyraCancelJob[SynthyraJobObject[data_Association], opts : OptionsPattern[]] := Module[{operation},
    operation = jobOperationName[data, {"CancelOperation", "CancellationOperation", "CancelOperationId", "cancelOperationId"}];
    If[MissingQ[operation],
        Return @ Failure[
            "CancellationUnsupported",
            <|"MessageTemplate" -> "The asynchronous operation does not declare a cancellation endpoint."|>
        ]
    ];
    SynthyraExecute[
        Lookup[data, "Client"],
        operation,
        jobRequest[data, operation],
        MaxRetries -> 0,
        Sequence @@ FilterRules[{opts}, DeleteCases[Options[SynthyraExecute], MaxRetries -> _]]
    ]
];
