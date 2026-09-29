(* SPDX-License-Identifier: Apache-2.0 *)
(* This file is generated. Do not edit by hand. *)

With[
    {generatedDirectory = DirectoryName[$InputFileName]},
    Scan[
        Get[FileNameJoin[{generatedDirectory, #}]] &,
        {"Schemas.wl", "Operations.wl", "Validators.wl"}
    ]
];
