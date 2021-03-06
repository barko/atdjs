open Printf
open Atd_ast

let rec mapi_sep f sep c accu = function
  | a :: b :: t ->
    let accu' = (sep c) :: (f c a) :: accu in
    mapi_sep f sep (c+1) accu' (b :: t);

  | [a] ->
    (f c a) :: accu;

  | [] -> accu

let mapi_sep f sep list =
  List.rev (mapi_sep f sep 0 [] list)

let map_sep f sep list =
  mapi_sep (fun c a -> f a) (fun c -> sep ()) list

let rec mapi f c accu = function
  | a :: t ->
    let accu' = (f c a) :: accu in
    mapi f (c+1) accu' t

  | [] -> List.rev accu

let mapi f list =
  mapi f 0 [] list

let sp = Printf.sprintf

let unpack_tuple element_prefix num_elements tuple_name =
  let elements = ref [] in
  for i = 0 to num_elements-1 do
    let element = element_prefix ^ (string_of_int i) in
    elements := element :: !elements
  done;
  let elements = List.rev !elements in
  let elements_s = String.concat ", " elements in
  sp "let %s  = %s in" elements_s tuple_name

let use_ocaml_array annot =
  List.fold_left (
    fun use_ocaml_array (section_name, (_, fields)) ->
      if section_name = "ocaml" then
        List.fold_left (
          fun use_ocaml_array_2 (key, (_, value_opt)) ->
            (key = "repr" && value_opt = Some "array") || use_ocaml_array_2
        ) use_ocaml_array fields
      else
        use_ocaml_array
  ) false annot

let err = "only support <ocaml module=\"Module\"> abstract annotations"
let module_name_of_abstract_type_annot = function
  | ["ocaml", ocaml_section] -> (
      match ocaml_section with
        | _, ["module", (_, Some module_name) ] -> module_name
        | _ -> failwith err
    )
  | _ ->
      failwith err

let field_prefix_of_record_annots annots =
    try
      let ocaml_section = List.assoc "ocaml" annots in
      match ocaml_section with
        | _, ["field_prefix", (_, Some field_prefix) ] -> Some field_prefix
        | _, [other, _] ->
            printf "ignoring annotation %s\n%!" other;
            None
        | _ ->
            None
    with Not_found ->
      None


(* functions to implement functions [json_of_{t}] *)
let let_keyword is_recursive item_num =
  if is_recursive then
    if item_num = 0 then
      "let rec"
    else
      "and"
  else
    "let"

let rec wr_module_item is_recursive item_num = function
  | `Type (_, (name, _, annot ), expr) -> [
      `Line (sp "%s json_of_%s add_s add_c %s =" (let_keyword is_recursive item_num)
               name name);
      `Block (wr_type_expr name annot expr);
    ]

and wr_type_expr name ty_annot = function
  | `Sum (_, variants, _) -> [
      `Line (sp "(match %s with" name);
      `Inline (List.map (fun variant -> `Block (wr_variant ty_annot variant)) variants);
      `Line ")"
    ]

  | `Record (_, fields, record_annots) ->
      let field_prefix_opt = field_prefix_of_record_annots record_annots in
      [
        `Line "add_s \"{\";";
        `Block (
          map_sep
            (fun field -> `Inline (wr_field name ty_annot field_prefix_opt field))
            (fun () -> `Line "; add_s \",\";") fields
        );
        `Line "; add_s \"}\""
      ]

  | `Tuple (_, cells, _) -> [
      `Line (unpack_tuple "c" (List.length cells) name);
      `Line "add_s \"[\"; ";
      `Block (
        mapi_sep (
          fun i cell ->
            let name = sprintf "c%d" i in
            `Inline (wr_cell name ty_annot cell)
        ) (fun _ -> `Line "; add_s \",\"; ") cells
      );
      `Line "; add_s \"]\""
    ]

  | `Name (_, (_, type_name, type_exprs), annot ) -> [
      match type_name with
        | "bool"     -> `Line (sp "add_s (string_of_bool %s)" name)
        | "int"      -> `Line (sp "add_s (string_of_int %s)" name)
        | "float"    -> `Line (sp "add_s (Json_io.string_of_json_float %s)" name)
        | "unit"     -> `Line (sp "add_s \"null\"")
        | "string"   -> `Line (
            sp "(add_s \"\\\"\"; escape add_s add_c %s; add_s \"\\\"\")" name
          )
        | "abstract" ->
            let module_name = module_name_of_abstract_type_annot ty_annot in
            `Line (sp "add_s (%s.string_of_%s %s)" module_name name name)
        | other -> `Line (sp "json_of_%s add_s add_c %s " type_name name)
    ]

  | `List (_, expr, annot) ->
      let iterator =
        if use_ocaml_array annot then
          "array_iter_sep"
        else
          "iter_sep"
      in
      [
        `Line "add_s \"[\";";
        `Line (iterator ^ " (");
        `Block [
          `Line "fun el -> ";
          `Block (wr_type_expr "el" ty_annot expr);
        ];
        `Line (sp ") (fun _ -> add_s \",\") %s;" name);
        `Line "add_s \"]\""
      ]

  | `Option (_, expr, _) -> [
      `Line (sp "match %s with" name);
      `Block [
        `Line "| Some x ->";
        `Block [
          `Line "add_s \"[\\\"Some\\\",\"; ";
          `Block (wr_type_expr "x" ty_annot expr);
          `Line "; add_s \"]\"";
        ];
        `Line "| None -> add_s \"\\\"None\\\"\""
      ]
    ]

  | `Wrap _ -> failwith "wrap values not supported"
  | `Nullable _ -> failwith "nullable values not supported"
  | `Shared _ -> failwith "shared values not supported"
  | `Tvar _ -> failwith "tvar not supported"

and wr_variant ty_annot = function
  | `Variant (_, (name,_), type_expr_opt) -> [
      `Block (
        match type_expr_opt with
          | Some type_expr -> [
              `Line (sp "| `%s x ->" name);
              `Block [
                `Line "add_s \"[\";";
                `Block [
                  `Line (sp "add_s \"\\\"%s\\\",\"; " name);
                  `Inline (wr_type_expr "x" ty_annot type_expr);
                ];
                `Line "; add_s \"]\" "
              ];
            ]
          | None ->
              [ `Line (sp "| `%s -> add_s \"\\\"%s\\\"\" " name name) ]
      )
    ]

  | `Inherit _ -> assert false

and wr_field record_name ty_annot field_prefix_opt = function
  | `Field (_, (field_name, field_kind, _), type_expr) ->
      (match field_kind with
         | `Required -> [
             `Line (sp "add_s \"\\\"%s\\\":\";" field_name);
             `Inline (
               let field_prefix =
                 match field_prefix_opt with
                   | Some field_prefix -> field_prefix
                   | None -> ""
               in
               let dot = record_name ^ "." ^ field_prefix ^ field_name in
               wr_type_expr dot ty_annot type_expr;
             )
           ]

         | `Optional -> failwith "optional fields not supported"
         | `With_default -> failwith "optional fields with default not supported"
      )

  | `Inherit _ -> assert false

and wr_cell name ty_annot (_, expr, _) =
  wr_type_expr name ty_annot expr


(* functions to implement functions [string_of_{t}], by calling
   [json_of_{t}] *)
let wr_js_module_item = function
  | `Type (_, (name, _, _), expr) -> [
    `Line (sp "let string_of_%s %s =" name name);
    `Block [
      `Line "let buf = Buffer.create 100 in";
      `Line "let add_s = Buffer.add_string buf in";
      `Line "let add_c = Buffer.add_char buf in";
      `Line (sp "json_of_%s add_s add_c %s;" name name);
      `Line "Buffer.contents buf"
    ]
  ]

(* functions to render type declarations *)
let rec ty_module_item is_recursive item_num = function
  | `Type (_, (name, _, ty_annot), expr) ->
      let type_keyword =
        if is_recursive then
          if item_num = 0 then
            "type"
          else
            "and"
        else
          "type"
      in [
        `Line (sp "%s %s =" type_keyword name);
        `Block (ty_type_expr ty_annot name expr)
      ]

and ty_type_expr ty_annot ty_name = function
  | `Sum (_, variants, _) -> [
      `Line "[";
      `Inline (
        List.map (
          fun variant ->
            `Inline (ty_variant ty_annot ty_name variant)
        ) variants
      );
      `Line "]";
    ]

  | `Record (_, fields, record_annots ) -> [
      `Line "{";
      `Inline (
        List.map (
          function
            | `Field (_, (field_name, field_kind, _), type_expr) ->
                let field_prefix =
                  match field_prefix_of_record_annots record_annots with
                    | Some field_prefix -> field_prefix
                    | None -> ""
                in

                (match field_kind with
                   | `Required -> `Block [
                       `Line (sp "%s%s : " field_prefix field_name);
                       `Inline (ty_type_expr ty_annot ty_name type_expr );
                       `Line ";"
                     ]

                   | `Optional -> failwith "optional fields not supported"
                   | `With_default -> failwith "optional fields with default not supported"
                )

            | `Inherit _ -> assert false

        ) fields
      );
      `Line "}"
    ]

  | `Name (_, (_, type_name, type_exprs), annot) -> [
      match type_name with
        | "abstract" ->
            let module_name = module_name_of_abstract_type_annot ty_annot in
            `Line (sp "%s.%s" module_name ty_name)

        | other -> `Line (type_name)
    ]

  | `Tuple (_, cells, _) -> [
      `Line "(";
      `Block (
        map_sep (
          fun (_, expr, _) ->
            `Inline (ty_type_expr ty_annot ty_name expr)
        ) (fun _ -> `Line "*") cells
      );
      `Line ")"
    ]

  | `List (_, expr, annot) -> [
      `Inline (ty_type_expr ty_annot ty_name expr);
      `Line (
        if use_ocaml_array annot then
          "array"
        else
          "list"

      )

    ]

  | `Option (_, expr, _) -> [
      `Inline (ty_type_expr ty_annot ty_name expr);
      `Line "option"
    ]

  | `Wrap _ -> failwith "wrap values not supported"
  | `Nullable _ -> failwith "nullable not supported"
  | `Shared _ -> failwith "shared values not supported"
  | `Tvar _ -> failwith "tvar not supported"

and ty_variant ty_annot ty_name = function
  | `Variant (_, (name,_), type_expr_opt) -> (
      match type_expr_opt with
        | Some type_expr -> [
            `Line (sp "| `%s of " name);
            `Block (ty_type_expr ty_annot ty_name type_expr)
          ]
        | None ->
            [`Line (sp "| `%s" name )]
    )

  | `Inherit _ -> assert false


(* functions to implement functions [{t}_of_json], using [jsonoj]'s
   JSON parser. *)
let rd_js_module_item = function
  | `Type (_, (name, _, _), expr) -> [
    `Line (sp "let %s_of_string %s =" name name);
    `Block [
      `Line (sp "let j = Json_io.json_of_string %s in" name);
      `Line (sp "%s_of_json j" name);
    ]
  ]

let rec rd_module_item is_recursive item_num = function
  | `Type (_, (name, _, annot), expr) -> [
      `Line (sp "%s %s_of_json %s =" (let_keyword is_recursive item_num) name name);
      `Block (rd_type_expr name annot expr);
    ]

and rd_type_expr name ty_annot = function
  | `Sum (_, variants, _) -> [
      `Line (sp "(match %s with" name);
      `Inline (List.map (fun variant -> `Block (rd_variant ty_annot variant)) variants);
      `Block [`Line "| _ -> raise Error"];
      `Line ")"
    ]

  | `Tuple (_, cells, _) ->
      let list_unpack = mapi (fun i _ -> "c" ^ (string_of_int i)) cells in
      let list_unpack_s = String.concat "; " list_unpack in
      [
        `Line (sp "(match %s with" name);
        `Block [
          `Line (sp "| Array [ %s ] -> (" list_unpack_s);
          `Block (
            mapi_sep
              (fun i (_, expr, _) ->
                 let c_name = "c" ^ (string_of_int i) in
                 `Inline (rd_type_expr c_name ty_annot expr)
              )
              (fun _ -> `Line ",") cells
          );
          `Line ")";
          `Line "| _ -> raise Error";
        ];
        `Line ")"
      ]

  | `Name (_, (_, type_name, type_exprs), annot) -> (
      let primitive line = [
        `Line (sp "(match %s with" name);
        `Block [
          `Line line;
          `Line "| _ -> raise Error"
        ];
        `Line ")"
      ] in

      match type_name with
        | "bool"     -> primitive "| Bool b -> b"
        | "int"      -> primitive "| Int i -> i"
        | "float"    -> primitive "| Float f -> f"
        | "unit"     -> primitive "| Null -> ()"
        | "string"   -> primitive "| String s -> s"

        | "abstract" ->
            let module_name = module_name_of_abstract_type_annot ty_annot in
            [`Line (sp "(%s.%s_of_json %s)" module_name name name )]

        | other -> [`Line (sp "(%s_of_json %s)" other name)]

    )

  | `Option (_, expr, _) -> [
      `Line (sp "(match %s with" name);
      `Block [
        `Line "| Array [String \"Some\"; z ] -> Some ";
        `Block (rd_type_expr "z" ty_annot expr);
        `Line "| String \"None\" -> None";
        `Line "| _ -> raise Error"
      ];
      `Line ")"
    ]

  | `Record (_, fields, record_annots) -> [
      `Line (sp "(match %s with" name);
      `Block [
        `Line "| Object kv_list -> ";
        `Block [
          `Line "let find k = try List.assoc k kv_list with Not_found -> raise Error in";
          `Line "{";
          `Block (
            List.map (
              function
                | `Field (_, (field_name, field_kind, _), type_expr) ->
                    let field_prefix =
                      match field_prefix_of_record_annots record_annots with
                        | Some field_prefix -> field_prefix
                        | None -> ""
                    in
                    (match field_kind with
                       | `Required -> `Inline [
                           `Line (sp "%s%s = (" field_prefix field_name);
                           `Block [
                             `Line (sp "let v = find %S in" field_name);
                             `Inline (rd_type_expr "v" ty_annot type_expr);
                           ];
                           `Line ");"
                         ]

                       | `Optional -> failwith "optional fields not supported"
                       | `With_default ->
                           failwith "optional fields with default not supported"
                    )

                | `Inherit _ -> assert false

            ) fields);
          `Line "}";
        ];
        `Line "| _ -> raise Error";
      ];
      `Line ")"
    ]

  | `List (_, expr, annot) ->
      [
        `Line (sp "(match %s with" name);
        `Block [
          (* TODO: kind of inefficient to [Array.of_list (List.map ...)] *)
          if use_ocaml_array annot then `Inline [
            `Line "| Array z -> Array.of_list (List.map (fun el -> ";
            `Block (rd_type_expr "el" ty_annot expr);
            `Line ") z)";
          ]
          else `Inline [
            `Line "| Array z -> List.map (fun el -> ";
            `Block (rd_type_expr "el" ty_annot expr);
            `Line ") z";
          ];
          `Line "| _ -> raise Error";
        ];
        `Line ")"
      ];

  | `Wrap _ -> failwith "wrap values not supported"
  | `Nullable _ -> failwith "nullable not supported"
  | `Shared _ -> failwith "shared values not supported"
  | `Tvar _ -> failwith "tvar not supported"

and rd_variant ty_annot = function
  | `Variant (_, (name,_), type_expr_opt) -> [
      `Inline (
        match type_expr_opt with
          | Some type_expr -> [
              `Line (sp "| Array [String %S; x] -> `%s (" name name);
              `Block (rd_type_expr "x" ty_annot type_expr);
              `Line ")"
            ]
          | None -> [
              `Line (sp "| String %S -> `%s" name name)
            ]
      );

    ]

  | `Inherit _ -> assert false

let wr_js_module_item = function
  | `Type (_, (name, _, _), expr) -> [
    `Line (sp "let string_of_%s %s =" name name);
    `Block [
      `Line "let buf = Buffer.create 100 in";
      `Line "let add_s = Buffer.add_string buf in";
      `Line "let add_c = Buffer.add_char buf in";
      `Line (sp "json_of_%s add_s add_c %s;" name name);
      `Line "Buffer.contents buf"
    ]
  ]

let sig_module_item = function
  | `Type (_, (name, _, _), _) -> [
    `Line (sp "val string_of_%s : %s -> string" name name);
    `Line (sp "val %s_of_string : string -> %s" name name);
    `Line (sp "val %s_of_json : Json_type.json_type -> %s" name name);
  ]

let bodies_of_module modu =
  List.flatten (
    List.rev (
      List.fold_left (
        fun accu (_, body) ->  body :: accu
      ) [] modu
    )
  )

let make_ocaml_files ~opens atd_file prefix =
  let head, m0 = Atd_util.load_file
    ~expand:false
    ~inherit_fields:true
    ~inherit_variants:true
    atd_file
  in
  let un_expanded = Atd_util.tsort m0 in
  let expanded = Atd_util.tsort (Atd_expand.expand_module_body ~keep_poly:true m0) in
  let items_un_expanded = bodies_of_module un_expanded in

  let empty_line () =
    `Line ""
  in

  let opens_code = map_sep (
    fun module_name ->
      `Line (sp "open %s" module_name);
  ) empty_line opens in

  let wr_js_ml_code = map_sep (
    fun item ->
      `Inline (wr_js_module_item item)
  ) empty_line items_un_expanded in

  let rd_js_ml_code = map_sep (
    fun item ->
      `Inline (rd_js_module_item item)
  ) empty_line items_un_expanded in

  let ml_code f =
    map_sep (
      fun (is_recursive, items) ->
        `Inline (
          List.flatten (
            mapi (
              fun i ->
                f is_recursive i
          ) items
        )
      )
    ) empty_line expanded
  in

  let wr_ml_code = ml_code wr_module_item in
  let rd_ml_code = ml_code rd_module_item in

  let ty_code (is_recursive, items) =
    `Inline (
      List.flatten (
        mapi (
          fun i ->
            ty_module_item is_recursive i
        ) items
      )
    )
  in

  let ty_ml_code = map_sep ty_code empty_line expanded in
  let ty_mli_code = map_sep ty_code empty_line un_expanded in

  let sig_mli_code = map_sep (
    fun item ->
      `Inline (sig_module_item item)
  ) empty_line items_un_expanded in

  let iter_sep_function = [
    `Line "let rec iter_sep f sep = function";
    `Block [
      `Line "| a :: b :: t ->";
      `Block [
        `Line "f a;";
        `Line "sep a;";
        `Line "iter_sep f sep (b :: t);";
        `Line "";
      ];
      `Line "| [a] -> f a";
      `Line "";
      `Line "| [] -> ()";
    ];
  ] in

  let array_iter_sep_function = [
    `Line "let array_iter_sep f sep a =";
    `Block [
      `Line "let len = Array.length a in";
      `Block [
        `Line "if len > 0 then (";
        `Block [
          `Line "for i = 0 to len - 2 do";
          `Block [
            `Line "let ai = a.(i) in";
            `Line "f ai;";
            `Line "sep ai;";
          ];
          `Line "done;";
          `Line "f a.(len-1)";
        ];
        `Line ")"
      ]
    ]
  ] in

  let map_sep_id_empty_line = map_sep (fun x -> x) empty_line in
  let ml_code = map_sep_id_empty_line [
    `Line "(* auto-generated by atdjs *)";
    `Line "let escape = Json_io.escape_json_string";

    `Inline opens_code;
    `Line "exception Error";
    `Line "open Json_type";
    `Inline iter_sep_function;
    `Inline array_iter_sep_function;

    `Inline ty_ml_code;
    `Inline wr_ml_code;
    `Inline rd_ml_code;
    `Inline wr_js_ml_code;
    `Inline rd_js_ml_code;
  ] in

  let mli_code = map_sep_id_empty_line [
    `Line "(* auto-generated by atdjs *)";
    `Line "exception Error";
    `Inline ty_mli_code;
    `Inline sig_mli_code;
  ] in

  let ml_ch, mli_ch =
    match prefix with
      | `Stdout -> stdout, stdout
      | `Files path -> open_out (path ^ ".ml"), open_out (path ^ ".mli")
  in

  Atd_indent.to_channel ml_ch ml_code;
  Atd_indent.to_channel mli_ch mli_code;
  close_out ml_ch;
  close_out mli_ch







