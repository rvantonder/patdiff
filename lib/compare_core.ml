open Core
open Import

module File = struct
  (** [displayed_name <> real_name] when the config specifies prev_alt or next_alt. *)
  type t =
    { displayed_name : string
    ; real_name : string
    }

  let append t string =
    { displayed_name = t.displayed_name ^/ string; real_name = t.real_name ^/ string }
  ;;

  let of_real (config : Configuration.t) which real_name =
    let config_displayed_name =
      match which with
      | `Prev -> config.prev_alt
      | `Next -> config.next_alt
    in
    { real_name; displayed_name = Option.value config_displayed_name ~default:real_name }
  ;;
end

let lines_of_contents contents =
  let lines = Array.of_list (String.split_lines contents) in
  let has_trailing_newline =
    let length = String.length contents in
    if length = 0 || Char.equal contents.[length - 1] '\n'
    then `With_trailing_newline
    else `Missing_trailing_newline
  in
  lines, has_trailing_newline
;;

let%test_unit _ =
  let test contents ~expect =
    [%test_result: string array * [ `With_trailing_newline | `Missing_trailing_newline ]]
      (lines_of_contents contents)
      ~expect
  in
  test "" ~expect:([||], `With_trailing_newline);
  test "hello" ~expect:([| "hello" |], `Missing_trailing_newline);
  test "hello\nworld" ~expect:([| "hello"; "world" |], `Missing_trailing_newline);
  test "hello\nworld\n" ~expect:([| "hello"; "world" |], `With_trailing_newline)
;;

let convert_to_patch_compatible_hunks ?prev_diff ?next_diff lines_prev lines_next hunks =
  let open Patdiff_core in
  let open Patience_diff.Hunk in
  let open Patience_diff.Range in
  if Option.is_none prev_diff || Option.is_none next_diff then
    ()
  else
    let prev = Option.value_exn prev_diff in
    let next = Option.value_exn next_diff in
    let has_trailing_newline text =
      try String.contains ~pos:(String.length text - 1) text '\n' with _ -> false
    in
    let prev_file_no_trailing_newline = not (has_trailing_newline prev.text) in
    let next_file_no_trailing_newline = not (has_trailing_newline next.text) in
    if prev_file_no_trailing_newline || next_file_no_trailing_newline then
      (* if file does not have newline, and start + size - 1 = number of lines in file without a newline, then add \... *)
      List.iter hunks ~f:(fun hunk ->
          if
            Array.length lines_prev = (hunk.prev_start + hunk.prev_size - 1)
            || Array.length lines_next = (hunk.next_start + hunk.next_size - 1)
          then
            let correction = "\n\\ No newline at end of file" in
            (* We can't just take the last because the second last might be a prev
               that needs the message too (because for unrefined patdiff converts
               the replace to a prev and a next). We can't just handle all ranges
               in this hunk either, because some prev or next might be earlier
               than the rest of the prev or nexts. *)
            List.last hunk.ranges
            |> function
              (* if last hunk range are the same *)
            | Some Same a ->
              let prev, next = Array.get a (Array.length a - 1) in
              Array.set a (Array.length a - 1) (prev^correction, next^correction)
            (* if the last hunk had things deleted *)
            | Some Prev a when prev_file_no_trailing_newline ->
              let prev = Array.get a (Array.length a - 1) in
              Array.set a (Array.length a - 1) (prev^correction)
            | Some Next a when next_file_no_trailing_newline ->
              (* Prev has a newline, dest does not *)
              let next = Array.get a (Array.length a - 1) in
              Array.set a (Array.length a - 1) (next^correction)
            | Some Unified a ->
              let unified = Array.get a (Array.length a - 1) in
              Array.set a (Array.length a - 1) (unified^correction)
            | Some Replace (a1, a2) ->
              if prev_file_no_trailing_newline then
                (
                  let prev = Array.get a1 (Array.length a1 - 1) in
                  Array.set a1 (Array.length a1 - 1) (prev^correction)
                );
              if next_file_no_trailing_newline then
                (
                  let next = Array.get a2 (Array.length a2 - 1) in
                  Array.set a2 (Array.length a2 - 1) (next^correction)
                )
            | _ -> ())

(* Returns a Hunk.t list, ready to be printed *)
let compare_lines (config : Configuration.t) ?prev_diff ?next_diff ~prev ~next () =
  let module C = Configuration in
  (* Create the diff *)
  let context = config.context in
  let keep_ws = config.keep_ws in
  let split_long_lines = config.split_long_lines in
  let line_big_enough = config.line_big_enough in
  let hunks =
    let transform = if keep_ws then Fn.id else Patdiff_core.remove_ws in
    (* Use external compare program? *)
    match config.ext_cmp with
    | None ->
      Patience_diff.String.get_hunks
        ~transform
        ~context
        ~big_enough:line_big_enough
        ~prev
        ~next
    | Some prog ->
      let compare x y =
        let cmd = sprintf "%s %S %S" prog x y in
        match Unix.system cmd with
        | Ok () -> 0
        | Error (`Exit_non_zero 1) -> 1
        | Error _ -> failwithf "External compare %S failed!" prog ()
      in
      let module P =
        Patience_diff.Make (struct
          type t = string [@@deriving sexp]

          let hash = String.hash
          let compare = compare
        end)
      in
      P.get_hunks ~transform ~context ~big_enough:line_big_enough ~prev ~next
  in
  let hunks =
    match config.float_tolerance with
    | None -> hunks
    | Some tolerance -> Float_tolerance.apply hunks tolerance ~context
  in
  (* Refine if desired *)
  if config.unrefined
  then
    (
      (* Before turning replace ranges into `Prev and `Next, add the diff fixups *)
      convert_to_patch_compatible_hunks ?prev_diff ?next_diff prev next hunks;
      (* Turn `Replace ranges into `Prev and `Next ranges.
         `Replace's would otherwise be later interpreted as refined output *)
      Patience_diff.Hunks.unified hunks
    )
  else (
    let rules = config.rules in
    let output = config.output in
    let produce_unified_lines = config.produce_unified_lines in
    let interleave = config.interleave in
    let word_big_enough = config.word_big_enough in
    Patdiff_core.refine
      ~rules
      ~output
      ~keep_ws
      ~produce_unified_lines
      ~split_long_lines
      ~interleave
      hunks
      ~word_big_enough)
;;

let warn_if_no_trailing_newline
    ~warn_if_no_trailing_newline_in_both
    (prev_file_newline, prev_file)
    (next_file_newline, next_file)
  =
  let warn = eprintf "No newline at the end of %s\n%!" in
  match prev_file_newline, next_file_newline with
  | `With_trailing_newline, `With_trailing_newline -> ()
  | `With_trailing_newline, `Missing_trailing_newline -> warn next_file
  | `Missing_trailing_newline, `With_trailing_newline -> warn prev_file
  | `Missing_trailing_newline, `Missing_trailing_newline ->
    if warn_if_no_trailing_newline_in_both
    then (
      warn prev_file;
      warn next_file)
;;

(* Returns a Hunk.t list, ready to be printed *)
let compare_files (config : Configuration.t) ~(prev_file : File.t) ~(next_file : File.t) =
  let prev = In_channel.read_all prev_file.real_name in
  let next = In_channel.read_all next_file.real_name in
  Comparison_result.create
    config
    ~prev:{ name = prev_file.displayed_name; text = prev }
    ~next:{ name = next_file.displayed_name; text = next }
    ~compare_assuming_text:(fun config ~prev ~next ->
        let prev_lines, prev_file_newline = lines_of_contents prev.text in
        let next_lines, next_file_newline = lines_of_contents next.text in
        warn_if_no_trailing_newline
          (prev_file_newline, prev.name)
          (next_file_newline, next.name)
          ~warn_if_no_trailing_newline_in_both:config.warn_if_no_trailing_newline_in_both;
        compare_lines config ~prev:prev_lines ~next:next_lines ())
;;

let binary_different_message
    ~(config : Configuration.t)
    ~prev_file
    ~prev_is_binary
    ~next_file
    ~next_is_binary
  =
  match config.location_style with
  | Diff ->
    sprintf
      "Files %s%s and %s%s differ"
      prev_file
      (if prev_is_binary then " (binary)" else "")
      next_file
      (if next_is_binary then " (binary)" else "")
  | Omake ->
    String.concat
      [ error_message_start ~file:prev_file ~line:1
      ; "\n"
      ; "  File \""
      ; next_file
      ; "\"\n"
      ; "  binary files differ\n"
      ]
;;

(* Print hunks to stdout *)
let print hunks ~(prev_file : File.t) ~(next_file : File.t) ~(config : Configuration.t) =
  if Comparison_result.has_no_diff hunks
  then (
    if config.double_check
    then (
      match
        Unix.system (sprintf "cmp -s %s %s" prev_file.real_name next_file.real_name)
      with
      | Ok () -> ()
      | Error (`Exit_non_zero 1) ->
        printf "There are no differences except those filtered by your settings\n%!"
      | Error _ -> ()))
  else if (* Only print if -quiet is not set *)
    not config.quiet
  then (
    let output = config.output in
    let rules = config.rules in
    match hunks with
    | Binary_same -> assert false
    | Binary_different { prev_is_binary; next_is_binary } ->
      Printf.printf
        "%s\n"
        (binary_different_message
           ~config
           ~prev_file:prev_file.displayed_name
           ~prev_is_binary
           ~next_file:next_file.displayed_name
           ~next_is_binary)
    | Hunks hunks ->
      Patdiff_core.print
        hunks
        ~prev_file:prev_file.displayed_name
        ~next_file:next_file.displayed_name
        ~output
        ~rules
        ~location_style:config.location_style)
;;

let diff_files config ~prev_file ~next_file =
  let prev_file = File.of_real config `Prev prev_file in
  let next_file = File.of_real config `Next next_file in
  let hunks = compare_files ~prev_file ~next_file config in
  print hunks ~prev_file ~next_file ~config;
  if Comparison_result.has_no_diff hunks then `Same else `Different
;;

(* Copied from string.ml, but preserves '\r's *)
let split_lines_preserve_slash_r =
  let back_up_at_newline ~t:_ ~pos ~eol =
    pos := !pos - 1;
    eol := !pos + 1;
  in
  fun t ->
    let n = String.length t in
    if n = 0
    then []
    else
      (* Invariant: [-1 <= pos < eol]. *)
      let pos = ref (n - 1) in
      let eol = ref n in
      let ac = ref [] in
      (* We treat the end of the string specially, because if the string ends with a
         newline, we don't want an extra empty string at the end of the output. *)
      if Char.equal t.[!pos] '\n' then back_up_at_newline ~t ~pos ~eol;
      while !pos >= 0 do
        if Char.( <> ) t.[!pos] '\n'
        then decr pos
        else
          (* Because [pos < eol], we know that [start <= eol]. *)
          let start = !pos + 1 in
          ac := String.sub t ~pos:start ~len:(!eol - start) :: !ac;
          back_up_at_newline ~t ~pos ~eol
      done;
      String.sub t ~pos:0 ~len:!eol :: !ac
;;

let diff_strings
    ?print_global_header
    (config : Configuration.t)
    ~(prev : Patdiff_core.diff_input)
    ~(next : Patdiff_core.diff_input)
  =
  let lines { Patdiff_core.name = _; text } = split_lines_preserve_slash_r text |> Array.of_list in
  let hunks =
    Comparison_result.create
      config
      ~prev
      ~next
      ~compare_assuming_text:(fun config ~prev ~next ->
          let lines_prev = lines prev in
          let lines_next = lines next in
          compare_lines config ~prev_diff:prev ~next_diff:next ~prev:lines_prev ~next:lines_next ())
  in
  if Comparison_result.has_no_diff hunks
  then `Same
  else
    `Different
      (match hunks with
       | Binary_same -> assert false
       | Binary_different { prev_is_binary; next_is_binary } ->
         binary_different_message
           ~config
           ~prev_file:prev.name
           ~prev_is_binary
           ~next_file:next.name
           ~next_is_binary
       | Hunks hunks ->
         Patdiff_core.output_to_string
           hunks
           ?print_global_header
           ~file_names:(prev.name, next.name)
           ~output:config.output
           ~rules:config.rules
           ~location_style:config.location_style)
;;

let is_reg path =
  match Unix.stat path with
  | { st_kind = S_REG; _ } -> true
  | _ -> false
;;

let is_dir path =
  match Unix.stat path with
  | { st_kind = S_DIR; _ } -> true
  | _ -> false
;;

let rec diff_dirs_internal
          (config : Configuration.t)
          ~(prev_dir : File.t)
          ~(next_dir : File.t)
          ~file_filter
  =
  assert (is_dir prev_dir.real_name);
  assert (is_dir next_dir.real_name);
  let set_of_dir (dir : File.t) =
    (* Get a list of files for this directory only; do not descend farther
       (We recursively call diff_dirs later if we need to descend.) *)
    let file_filter =
      match file_filter with
      | None -> Fn.const true
      | Some file_filter -> file_filter
    in
    Sys.ls_dir dir.real_name
    |> List.filter ~f:(fun x ->
      let x = dir.real_name ^/ x in
      match Unix.stat x with
      | exception Unix.Unix_error (ENOENT, _, _) ->
        (* If the file disappeared during listing, let's pretend it didn't exist.
           This is important when the file is [-exclude]d because we don't want to create
           noise for excluded files, but it's also not too bad if the file is [-include]d
        *)
        false
      | stats -> file_filter (x, stats))
    |> String.Set.of_list
  in
  let prev_set = set_of_dir prev_dir in
  let next_set = set_of_dir next_dir in
  (* Get unique files *)
  let union = Set.union prev_set next_set in
  let prev_uniques = Set.diff union next_set in
  let next_uniques = Set.diff union prev_set in
  let handle_unique which file ~(dir : File.t) =
    printf "Only in %s: %s\n%!" dir.displayed_name file;
    (* Diff unique files against /dev/null, if desired *)
    if not config.mask_uniques
    then (
      let path = dir.real_name ^/ file in
      if is_reg path
      then (
        let diff = diff_files config in
        let null = "/dev/null" in
        match which with
        | `Prev -> ignore (diff ~prev_file:path ~next_file:null : [ `Different | `Same ])
        | `Next -> ignore (diff ~prev_file:null ~next_file:path : [ `Different | `Same ])))
  in
  Set.iter prev_uniques ~f:(handle_unique `Prev ~dir:prev_dir);
  Set.iter next_uniques ~f:(handle_unique `Next ~dir:next_dir);
  (* Get differences *)
  let inter = Set.inter prev_set next_set in
  let exit_code = ref `Same in
  let diff file =
    let prev_file = File.append prev_dir file in
    let next_file = File.append next_dir file in
    if is_reg prev_file.real_name && is_reg next_file.real_name
    then (
      let hunks = compare_files ~prev_file ~next_file config in
      if not (Comparison_result.has_no_diff hunks)
      then (
        exit_code := `Different;
        (* Print the diff if not -quiet *)
        match config.quiet with
        | false -> print hunks ~prev_file ~next_file ~config
        | true ->
          printf
            "Files %s and %s differ\n%!"
            prev_file.displayed_name
            next_file.displayed_name))
    else if is_dir prev_file.real_name && is_dir next_file.real_name
    then
      if not config.shallow
      then (
        match
          diff_dirs_internal ~prev_dir:prev_file ~next_dir:next_file config ~file_filter
        with
        | `Same -> ()
        | `Different -> exit_code := `Different)
      else
        printf
          "Common subdirectories: %s and %s\n%!"
          prev_file.displayed_name
          next_file.displayed_name
    else (
      exit_code := `Different;
      printf
        "Files %s and %s are not the same type\n%!"
        prev_file.displayed_name
        next_file.displayed_name)
  in
  Set.iter inter ~f:diff;
  if Set.is_empty prev_uniques && Set.is_empty next_uniques
  then !exit_code
  else `Different
;;

let diff_dirs config ~prev_dir ~next_dir ~file_filter =
  if not (is_dir prev_dir)
  then invalid_argf "diff_dirs: prev_dir '%s' is not a directory" prev_dir ();
  if not (is_dir next_dir)
  then invalid_argf "diff_dirs: next_dir '%s' is not a directory" next_dir ();
  let prev_dir = File.of_real config `Prev prev_dir in
  let next_dir = File.of_real config `Next next_dir in
  diff_dirs_internal config ~prev_dir ~next_dir ~file_filter
;;
