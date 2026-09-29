(* Lossless decoding for row-major uint8 matrix payloads. *)

matrixFailure[tag_String, message_String, data_: <||>] := Failure[
    tag,
    Join[<|"MessageTemplate" -> message|>, data]
];

matrixPayloadValue[payload_Association, names_List, default_: Missing["NotAvailable"]] :=
    associationLookup[payload, names, default];

decodeMatrixBytes[value_] := Module[{decoded},
    Which[
        Head[value] === ByteArray, value,
        StringQ[value],
            decoded = Quiet[Check[BaseDecode[value], $Failed]];
            If[Head[decoded] === ByteArray, decoded, $Failed],
        ListQ[value] && VectorQ[value, IntegerQ] && AllTrue[value, Between[{0, 255}]],
            ByteArray[value],
        True, $Failed
    ]
];

matrixIdentifiers[payload_Association, axis_String] := Module[{specific, candidates},
    specific = If[
        axis === "Row",
        {"row_ids", "rowIds", "ids_a", "idsA"},
        {"column_ids", "columnIds", "col_ids", "colIds", "ids_b", "idsB"}
    ];
    candidates = matrixPayloadValue[payload, specific, Missing["NotAvailable"]];
    If[! MissingQ[candidates], Return[candidates]];
    matrixPayloadValue[payload, {"candidate_ids", "candidateIds"}, Missing["NotAvailable"]]
];

fullProteomeExpectedIDs[payload_Association, rowIDs_List] := Module[
    {queryIDs, referenceIDs, proteomeIDs, queryCount, proteomeCount, joined},
    queryIDs = matrixPayloadValue[payload, {"query_ids", "queryIds"}, Missing["NotAvailable"]];
    referenceIDs = matrixPayloadValue[
        payload,
        {"reference_proteome_ids", "referenceProteomeIds"},
        Missing["NotAvailable"]
    ];
    proteomeIDs = matrixPayloadValue[payload, {"proteome_ids", "proteomeIds"}, Missing["NotAvailable"]];
    queryCount = matrixPayloadValue[payload, {"query_count", "queryCount"}, Missing["NotAvailable"]];
    proteomeCount = matrixPayloadValue[payload, {"proteome_count", "proteomeCount"}, Missing["NotAvailable"]];
    If[
        ! MissingQ[queryCount] || ! MissingQ[proteomeCount],
        If[
            ! IntegerQ[queryCount] || ! IntegerQ[proteomeCount] ||
                queryCount < 0 || proteomeCount < 0 ||
                queryCount + proteomeCount =!= Length[rowIDs],
            Return[Missing["NotAvailable"]]
        ]
    ];
    If[! ListQ[queryIDs],
        Return @ Which[
            ListQ[proteomeIDs] && proteomeIDs === rowIDs, proteomeIDs,
            IntegerQ[queryCount] && IntegerQ[proteomeCount] && queryCount >= 0 && proteomeCount >= 0 && queryCount + proteomeCount === Length[rowIDs], rowIDs,
            True, Missing["NotAvailable"]
        ]
    ];
    Which[
        ListQ[referenceIDs], Join[queryIDs, referenceIDs],
        ListQ[proteomeIDs] && proteomeIDs === rowIDs && Take[proteomeIDs, UpTo[Length[queryIDs]]] === queryIDs,
            proteomeIDs,
        ListQ[proteomeIDs],
            joined = Join[queryIDs, proteomeIDs];
            joined,
        True, Missing["NotAvailable"]
    ]
];

fullProteomeContractQ[payload_Association] := Module[{contract, mode, flag},
    contract = canonicalName @ matrixPayloadValue[payload, {"matrix_contract", "matrixContract"}, ""];
    mode = canonicalName @ matrixPayloadValue[payload, {"actome_mode", "actomeMode", "mode"}, ""];
    flag = matrixPayloadValue[payload, {"full_proteome_matrix", "fullProteomeMatrix"}, False];
    (! MissingQ[matrixPayloadValue[payload, {"query_count", "queryCount"}]] &&
        ! MissingQ[matrixPayloadValue[payload, {"proteome_count", "proteomeCount"}]]) ||
        MemberQ[{"fullproteome", "proteomefullmatrix", "qpbyqp"}, contract] ||
        mode === "proteome" || TrueQ[flag]
];

coreMatrixKeys = canonicalName /@ {
    "data", "scores_b64", "scoresB64", "matrix_b64", "matrixB64",
    "proteome_scores_b64", "proteomeScoresB64",
    "candidate_matrix_b64", "candidateMatrixB64", "shape", "matrix_shape",
    "matrixShape", "candidate_matrix_shape", "candidateMatrixShape", "row_ids",
    "rowIds", "column_ids", "columnIds", "col_ids", "colIds", "ids_a",
    "idsA", "ids_b", "idsB", "candidate_ids", "candidateIds"
};

matrixMetadata[payload_Association] := Association @ Select[
    Normal[payload],
    ! MemberQ[coreMatrixKeys, canonicalName[First[#]]] &
];

SynthyraScoreMatrix[payload_Association] := Module[
    {
        encoded, candidateEncoded, genericEncoded, proteomeEncoded,
        shape, rowIDs, columnIDs, bytes, values, matrix, expectedIDs, metadata,
        candidateWireQ, proteomeWireQ, proteomeIDs, proteomeBytes, proteomeValues,
        proteomeScores = Missing["NotAvailable"], dtype, encoding, order, result
    },
    candidateEncoded = matrixPayloadValue[
        payload,
        {"candidate_matrix_b64", "candidateMatrixB64"},
        Missing["NotAvailable"]
    ];
    genericEncoded = matrixPayloadValue[
        payload,
        {"data", "scores_b64", "scoresB64", "matrix_b64", "matrixB64"},
        Missing["NotAvailable"]
    ];
    proteomeEncoded = matrixPayloadValue[
        payload,
        {"proteome_scores_b64", "proteomeScoresB64"},
        Missing["NotAvailable"]
    ];
    candidateWireQ = ! MissingQ[candidateEncoded];
    proteomeWireQ = ! candidateWireQ && MissingQ[genericEncoded] && ! MissingQ[proteomeEncoded];
    encoded = Which[
        candidateWireQ, candidateEncoded,
        ! MissingQ[genericEncoded], genericEncoded,
        proteomeWireQ, proteomeEncoded,
        True, Missing["NotAvailable"]
    ];
    If[MissingQ[encoded],
        Return @ matrixFailure[
            "MissingMatrixData",
            "The score-matrix payload must contain base64 uint8 data."
        ]
    ];

    dtype = matrixPayloadValue[payload, {"dtype", "data_type", "dataType"}, Missing["NotAvailable"]];
    encoding = matrixPayloadValue[payload, {"encoding"}, Missing["NotAvailable"]];
    order = matrixPayloadValue[payload, {"order"}, Missing["NotAvailable"]];
    If[! MissingQ[dtype] && ! MemberQ[{"uint8", "unsignedinteger8"}, canonicalName[dtype]],
        Return @ matrixFailure["UnsupportedMatrixDataType", "Only lossless uint8 score matrices are supported."]
    ];
    If[! MissingQ[encoding] && canonicalName[encoding] =!= "base64",
        Return @ matrixFailure["UnsupportedMatrixEncoding", "Only base64 matrix payload encoding is supported."]
    ];
    If[! MissingQ[order] && canonicalName[order] =!= "rowmajor",
        Return @ matrixFailure["UnsupportedMatrixOrder", "Only row-major matrix payloads are supported."]
    ];

    bytes = decodeMatrixBytes[encoded];
    If[bytes === $Failed,
        Return @ matrixFailure[
            "InvalidMatrixEncoding",
            "The score-matrix data is not valid base64 or uint8 data."
        ]
    ];
    values = Normal[bytes];

    shape = matrixPayloadValue[
        payload,
        {"shape", "matrix_shape", "matrixShape", "candidate_matrix_shape", "candidateMatrixShape"},
        Missing["NotAvailable"]
    ];
    rowIDs = matrixIdentifiers[payload, "Row"];
    columnIDs = matrixIdentifiers[payload, "Column"];
    If[proteomeWireQ,
        proteomeIDs = matrixPayloadValue[payload, {"proteome_ids", "proteomeIds"}, Missing["NotAvailable"]];
        If[MissingQ[rowIDs] && ListQ[proteomeIDs], rowIDs = proteomeIDs];
        If[MissingQ[columnIDs] && ListQ[proteomeIDs], columnIDs = proteomeIDs]
    ];
    If[
        ! MatchQ[shape, {_Integer?(# >= 0 &), _Integer?(# >= 0 &)}],
        Return @ matrixFailure[
            "InvalidMatrixShape",
            "The score-matrix payload must declare an explicit two-dimensional nonnegative shape.",
            <|"Shape" -> shape|>
        ]
    ];
    If[! ListQ[rowIDs] || ! ListQ[columnIDs],
        Return @ matrixFailure[
            "MissingMatrixIdentifiers",
            "The score-matrix payload must declare row and column identifiers."
        ]
    ];
    If[Length[rowIDs] =!= shape[[1]] || Length[columnIDs] =!= shape[[2]],
        Return @ matrixFailure[
            "MatrixIdentifierShapeMismatch",
            "Row and column identifier counts must exactly match the declared shape.",
            <|
                "Shape" -> shape,
                "RowIdentifierCount" -> Length[rowIDs],
                "ColumnIdentifierCount" -> Length[columnIDs]
            |>
        ]
    ];

    If[Length[values] =!= Times @@ shape,
        Return @ matrixFailure[
            "MatrixByteCountMismatch",
            "The decoded uint8 byte count must exactly equal the product of the declared shape.",
            <|"Shape" -> shape, "ByteCount" -> Length[values]|>
        ]
    ];

    If[fullProteomeContractQ[payload],
        If[shape[[1]] =!= shape[[2]],
            Return @ matrixFailure[
                "FullProteomeMatrixNotSquare",
                "A full proteome-mode matrix must have shape (Q+P) by (Q+P).",
                <|"Shape" -> shape|>
            ]
        ];
        expectedIDs = fullProteomeExpectedIDs[payload, rowIDs];
        If[
            MissingQ[expectedIDs] || rowIDs =!= expectedIDs || columnIDs =!= expectedIDs,
            Return @ matrixFailure[
                "FullProteomeMatrixIdentifierMismatch",
                "Full proteome-mode row and column IDs must both equal the concatenated query and proteome IDs."
            ]
        ]
    ];

    If[candidateWireQ && ! MissingQ[proteomeEncoded],
        proteomeIDs = matrixPayloadValue[payload, {"proteome_ids", "proteomeIds"}, Missing["NotAvailable"]];
        proteomeBytes = decodeMatrixBytes[proteomeEncoded];
        If[proteomeBytes === $Failed,
            Return @ matrixFailure[
                "InvalidProteomeScoreEncoding",
                "The expansion-envelope proteome score vector is not valid base64 uint8 data."
            ]
        ];
        proteomeValues = Normal[proteomeBytes];
        If[! ListQ[proteomeIDs] || Length[proteomeValues] =!= Length[proteomeIDs],
            Return @ matrixFailure[
                "ProteomeScoreIdentifierMismatch",
                "The expansion-envelope proteome score count must exactly match the proteome identifier count.",
                <|
                    "ScoreCount" -> Length[proteomeValues],
                    "ProteomeIdentifierCount" -> If[ListQ[proteomeIDs], Length[proteomeIDs], Missing["NotAvailable"]]
                |>
            ]
        ];
        proteomeScores = NumericArray[proteomeValues, "UnsignedInteger8"]
    ];

    matrix = If[
        Times @@ shape === 0,
        NumericArray[{}, "UnsignedInteger8"],
        NumericArray[Partition[values, shape[[2]]], "UnsignedInteger8"]
    ];
    metadata = matrixMetadata[payload];
    result = <|
        "Matrix" -> matrix,
        "RowIDs" -> rowIDs,
        "ColumnIDs" -> columnIDs,
        "Shape" -> shape,
        "Encoding" -> "row-major uint8",
        "Metadata" -> metadata,
        "OriginalPayload" -> payload
    |>;
    If[! MissingQ[proteomeScores],
        AssociateTo[result, <|
            "ProteomeScores" -> proteomeScores,
            "ProteomeIDs" -> proteomeIDs
        |>]
    ];
    result
];

SynthyraScoreMatrix[other_] := matrixFailure[
    "InvalidMatrixPayload",
    "SynthyraScoreMatrix expects an Association payload.",
    <|"InputHead" -> Head[other]|>
];
