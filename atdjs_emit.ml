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

(* functions to implement functions [json_of_{t}] *)
let rec wr_module_item = function
  | `Type (_, (name, _, _), expr) -> [
    `Line (sp "let json_of_%s add_s add_c %s =" name name);
    `Block (wr_type_expr name expr);
  ]

and wr_type_expr name = function
  | `Sum (_, variants, _) -> [
    `Line (sp "(match %s with" name);
    `Inline (List.map (fun variant -> `Block (wr_variant variant)) variants);
    `Line ")"
  ]

  | `Record (_, fields, _) -> [
    `Line "add_s \"{\";";
    `Block (
      map_sep 
        (fun field -> `Inline (wr_field name field))
        (fun () -> `Line "add_s \",\";") fields
    );
    `Line "add_s \"}\""
  ]

  | `Tuple (_, cells, _) -> [
    `Line (unpack_tuple "c" (List.length cells) name);
    `Line "add_s \"[\"; ";
    `Block (
      mapi_sep (
        fun i cell -> 
          let name = sprintf "c%d" i in
          `Inline (wr_cell name cell)
      ) (fun _ -> `Line "add_s \",\"; ") cells
    );
    `Line "add_s \"]\""
  ]

  | `Name (_, (_, type_name, type_exprs), _) -> [
    match type_name with
      | "bool"     -> `Line (sp "add_s (string_of_bool %s) ; " name)
      | "int"      -> `Line (sp "add_s (string_of_int %s) ; " name)
      | "float"    -> `Line (sp "add_s (Json_io.string_of_json_float %s) ; " name)
      | "unit"     -> `Line (sp "add_s \"null\"; ")
      | "string"   -> `Line (
        sp "add_s \"\\\"\"; escape add_s add_c %s; add_s \"\\\"\"; " name
      )
      | "abstract" -> failwith "abstract field not supported"
      | other -> `Line (sp "json_of_%s add_s add_c %s; " type_name name)
  ]

  | `List (_, expr, _) -> [
    `Line "add_s \"[\";";
    `Line "iter_sep (";
    `Block [
      `Line "fun el -> ";
      `Block (wr_type_expr "el" expr);
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
        `Block (wr_type_expr "x" expr);
        `Line "add_s \"]\"";
      ];
      `Line "| None -> add_s \"\\\"None\\\"\"" 
    ]
  ]

  | `Shared _ -> failwith "shared values not supported"

  | `Tvar _ -> failwith "tvar not supported"

and wr_variant = function
  | `Variant (_, (name,_), type_expr_opt) -> [
    `Block (
      match type_expr_opt with
        | Some type_expr -> [
          `Line (sp "| `%s x ->" name);
          `Block [
            `Line "add_s \"[\";";
            `Block [
              `Line (sp "add_s \"\\\"%s\\\",\"; " name);
              `Inline (wr_type_expr "x" type_expr);
            ];
            `Line "add_s \"]\" "
          ];
        ]
        | None ->
          [ `Line (sp "| `%s -> add_s \"\\\"%s\\\"\" " name name) ]
    )
  ]

  | `Inherit _ -> assert false

and wr_field record_name = function
  | `Field (_, (field_name, field_kind, _), type_expr) -> 
    (match field_kind with
      | `Required -> [
        `Line (sp "add_s \"\\\"%s\\\":\";" field_name);
        `Inline (
          let dot = record_name ^ "." ^ field_name in
          wr_type_expr dot type_expr;
        )
      ]

      | `Optional -> failwith "optional fields not supported"
      | `With_default -> failwith "optional fields with default not supported"
    )

  | `Inherit _ -> assert false

and wr_cell name (_, expr, _) = 
  wr_type_expr name expr 


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
let rec ty_module_item = function
  | `Type (_, (name, _, _), expr) -> [
    `Line (sp "type %s =" name);
    `Block (ty_type_expr expr)
  ]

and ty_type_expr = function
  | `Sum (_, variants, _) -> [
    `Line "[";
    `Inline (
      List.map (
        fun variant -> 
          `Inline (ty_variant variant)
      ) variants
    );
    `Line "]";
  ]

  | `Record (_, fields, _) -> [
    `Line "{";
    `Inline (
      List.map (
        function 
          | `Field (_, (field_name, field_kind, _), type_expr) -> 
            (match field_kind with
              | `Required -> `Block [
                `Line (sp "%s : " field_name);
                `Inline (ty_type_expr type_expr);
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

  | `Name (_, (_, type_name, type_exprs), _) -> [
    match type_name with
      | "abstract" -> failwith "abstract field not supported"
      | other -> `Line (type_name)
  ]

  | `Tuple (_, cells, _) -> [
    `Line "(";
    `Block (
      map_sep (
        fun (_, expr, _) ->
          `Inline (ty_type_expr expr)
      ) (fun _ -> `Line "*") cells
    );
    `Line ")"
  ]

  | `List (_, expr, _) -> [
    `Inline (ty_type_expr expr);
    `Line "list"
  ]

  | `Option (_, expr, _) -> [
    `Inline (ty_type_expr expr);
    `Line "option"
  ]

  | `Shared _ -> failwith "shared values not supported"
  | `Tvar _ -> failwith "tvar not supported"

and ty_variant = function
  | `Variant (_, (name,_), type_expr_opt) -> (
    match type_expr_opt with
      | Some type_expr -> [
        `Line (sp "| `%s of " name);
        `Block (ty_type_expr type_expr)
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
    
let rec rd_module_item = function
  | `Type (_, (name, _, _), expr) -> [
    `Line (sp "let %s_of_json %s =" name name);
    `Block (rd_type_expr name expr);
  ]

and rd_type_expr name = function
  | `Sum (_, variants, _) -> [
    `Line (sp "(match %s with" name);
    `Inline (List.map (fun variant -> `Block (rd_variant variant)) variants);
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
              `Inline (rd_type_expr c_name expr)
            ) 
            (fun _ -> `Line ",") cells
        );
        `Line ")";
        `Line "| _ -> raise Error";
      ];
      `Line ")"
    ]

  | `Name (_, (_, type_name, type_exprs), _) -> (
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
      | "abstract" -> failwith "abstract field not supported"
      | other      -> [`Line (sp "%s_of_json %s" other name)]

  )

  | `Option (_, expr, _) -> [
    `Line (sp "(match %s with" name);
    `Block [
      `Line "| Array [String \"Some\"; z ] -> Some ";
      `Block (rd_type_expr "z" expr);
      `Line "| String \"None\" -> None";
      `Line "| _ -> raise Error"
    ];
    `Line ")"
  ]

  | `Record (_, fields, _) -> [
    `Line (sp "(match %s with" name);
    `Block [
      `Line "| Object kv_list -> ";
      `Block [
        `Line "let find k = try List.assoc k kv_list with Not_found -> raise Error in";
        `Line "{";
        `Block (List.map (
          function 
            | `Field (_, (field_name, field_kind, _), type_expr) -> 
              (match field_kind with
                | `Required -> `Inline [
                  `Line (sp "%s = (" field_name);
                  `Block [
                    `Line (sp "let v = find %S in" field_name);
                    `Inline (rd_type_expr "v" type_expr); 
                  ];
                  `Line ");"
                ]

                | `Optional -> failwith "optional fields not supported"
                | `With_default -> failwith "optional fields with default not supported"
              )

            | `Inherit _ -> assert false

        ) fields);
        `Line "}";
      ];
      `Line "| _ -> raise Error";
    ];
    `Line ")"
  ]

  | `List (_, expr, _) -> [
    `Line (sp "(match %s with" name);
    `Block [
      `Line "| Array z -> List.map (fun el -> ";
      `Block (rd_type_expr "el" expr);
      `Line ") z";
      `Line "| _ -> raise Error";
    ];
    `Line ")"
  ];
    

  | `Shared _ -> failwith "shared values not supported"
  | `Tvar _ -> failwith "tvar not supported"

and rd_variant = function
  | `Variant (_, (name,_), type_expr_opt) -> [
    `Inline (
      match type_expr_opt with
        | Some type_expr -> [
          `Line (sp "| Array [String %S; x] -> `%s (" name name);
          `Block (rd_type_expr "x" type_expr);
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
  ]


let make_ocaml_files ~opens atd_file prefix =
  let head, m0 = Atd_util.load_file 
    ~expand:false 
    ~inherit_fields:true 
    ~inherit_variants:true
    atd_file
  in
  let m2 = Atd_util.tsort (Atd_expand.expand_module_body ~keep_poly:true m0) in
  let bodies = 
    List.rev (
      List.fold_left (
        fun accu (_, body) ->  body :: accu
      ) [] m2
    ) 
  in
  let items2 = List.flatten bodies in

  let empty_line () =
    `Line ""
  in

  let wr_js_ml_code = map_sep (
    fun item ->
      `Inline (wr_js_module_item item)
  ) empty_line m0 in

  let rd_js_ml_code = map_sep (
    fun item ->
      `Inline (rd_js_module_item item)
  ) empty_line m0 in

  let wr_ml_code = map_sep (
    fun item ->
      `Inline (wr_module_item item)
  ) empty_line items2 in

  let rd_ml_code = map_sep (
    fun item ->
      `Inline (rd_module_item item)
  ) empty_line items2 in    

  let ty_ml_code = map_sep (
    fun item ->
      `Inline (ty_module_item item)
  ) empty_line items2 in

  let ty_mli_code = map_sep (
    fun item ->
      `Inline (ty_module_item item)
  ) empty_line m0 in

  let sig_mli_code = map_sep (
    fun item ->
      `Inline (sig_module_item item)
  ) empty_line m0 in

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

  let ml_code = [ 
    `Line "(* auto-generated by atdjs *)";
    `Line "let escape = Json_io.escape_json_string";
    `Line "exception Error";
    `Line "";
    `Line "open Json_type";

    `Inline iter_sep_function;
    `Line "";

    `Inline ty_ml_code;
    `Line "";

    `Inline wr_ml_code; 
    `Line "";

    `Inline rd_ml_code; 
    `Line "";

    `Inline wr_js_ml_code;
    `Line "";

    `Inline rd_js_ml_code;
    `Line "";
  ] in

  let mli_code = [
    `Line "(* auto-generated by atdjs *)";
    `Line "exception Error";
    `Line "";

    `Inline ty_mli_code;
    `Line "";

    `Inline sig_mli_code;
    `Line "";
  ] in

  let ml_ch, mli_ch = 
    match prefix with
      | `Stdout -> stdout, stdout
      | `Files path -> open_out (path ^ ".ml"), open_out (path ^ ".mli")
  in

  Atd_indent.to_channel ml_ch ml_code;
  Atd_indent.to_channel mli_ch mli_code
  



    


