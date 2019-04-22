open Format

let (|>) a f = f a
let fpf = fprintf
let efmt = err_formatter

module File = String
module Filename = struct
  include Filename

  let root = ref None
  let fdesc = ref (Some "\\\\")
  let dir_sep_origin = dir_sep

  let dir_sep() =
    match !fdesc with
    | None -> dir_sep_origin
    | Some s -> s
end

module Ext = String
exception IGNORE

module Files = Set.Make(File)
module FileExtMap = struct
  include Map.Make(File)

  let add filename m =
    let ext = Filename.extension filename in
    let files =
      if mem ext m then find ext m else Files.empty
    in
    let files = Files.add filename files in
    add ext files m
end

let verbose = ref false
let has_error_case = ref false
let html = ref true

let set_root s =
    Filename.root := Some s

let set_slash s =
    Filename.fdesc := (
      function
      | "" -> None
      | s -> Some s
    ) s

let patch_slash (s:string) : string =
  let s =
    Str.global_replace
      (Str.regexp (Filename.dir_sep_origin ^ Filename.dir_sep_origin))
    Filename.dir_sep_origin s
  in
  Str.global_replace (Str.regexp Filename.dir_sep_origin)
    (Filename.dir_sep()) s

let patch_path (path:string) : string =
  match !Filename.root with
  | Some v ->
    let path =
      if String.length path > 1 && 
        String.sub path 0 2 = ("." ^ Filename.dir_sep_origin) then
        String.sub path 2 (String.length path - 2)
      else if String.sub path 0 1 = "." then
        String.sub path 1 (String.length path - 1)
      else path
    in
    let v =
      if String.sub v 0 1 = Filename.dir_sep_origin then
        String.sub v 0 (String.length path - 1)
      else v
    in
    v ^ Filename.dir_sep_origin ^ path |> patch_slash
  | None -> patch_slash path

module Line = struct
  type t = int
  let compare = Pervasives.compare
end

module StrMap = Map.Make(String)
module StrSet = Set.Make(String)

let m1 = ref StrMap.empty
let m2 = ref StrMap.empty
let s3 = ref StrSet.empty

exception NotSupported of string
exception Failed of string * int

exception UnknownFile of string
exception UnknownExt of string * string

module FileInfo = struct
  type filename = string

  type filetype =
    | Known of string
    | UnkF
    | UnkE

  type t =
    { path:string; base:string; checksum:string;
      tm:Unix.tm; size:int; line:int; typ:string; }

  let compare t1 t2 =
    match Pervasives.compare t1.path t2.path with
    | 0 -> Pervasives.compare t1.checksum t2.checksum
    | n -> n

  let get_type base ext =
    let ext = String.lowercase_ascii ext in
    if StrSet.mem ext !s3 then
      raise IGNORE
    else if ext = "" then
      let base = String.lowercase_ascii base in
      match StrMap.find_opt base !m2 with
      | Some v -> if !html then Known v else Known ("\'" ^ v ^ "\'")
      | None ->
        UnkE
        (* raise (UnknownExt (base, ext)) *)
    else
      match StrMap.find_opt ext !m1 with
      | Some v -> if !html then Known v else Known ("\'" ^ v ^ "\'")
      | None ->
        UnkF
        (* raise (UnknownFile base) *)

  let count_lines_of f =
    (* let lines = ref 0 in 
    let chnl = open_in f in
    try
      while true do
        input_line chnl |> ignore;
        incr lines
      done; close_in chnl; 0
    with
    | End_of_file -> close_in chnl; !lines *)
    let chnl = open_in f in
    let strm = Stream.of_channel chnl in
    let lines = ref 1 in
    Stream.iter (function
      | '\n' -> incr lines
      | _ -> () 
    ) strm;
    close_in chnl;
    !lines

  let gen ?(safe=true) filename =
    let path = Filename.dirname filename in
    let base = Filename.basename filename in
    (* let size =
      let ic = open_in filename in
      let size = in_channel_length ic in
      close_in ic; size
    in *)
    let stat = Unix.stat filename in
    let size = stat.Unix.st_size in
    let checksum  = Digest.file filename |> Digest.to_hex in
    let tm = Unix.localtime stat.Unix.st_mtime in
    let line = count_lines_of filename in
    let ext = Filename.extension filename in
    let typ =
      match get_type base ext with
      | UnkE | UnkF ->
        if safe then
          if !html then "Misc file" else "'Misc file'"
        else raise (UnknownFile filename)
      | Known typ -> typ
    in
    { path; base; checksum; line; tm; size; typ }

  let pp ?(nopath=false) fmt t =
    if nopath then
      fpf fmt "%s\t" t.base
    else
      fpf fmt "%s/%s\t" t.path t.base;
    fpf fmt "%d\t%s\t" t.size (String.sub t.checksum 0 8);
    fpf fmt "%d.%0.d.%0.d\t%d\t%s" 
      (t.tm.Unix.tm_year + 1900)
      (t.tm.Unix.tm_mon + 1) 
      t.tm.Unix.tm_mday
      t.line t.typ

  let pp_html i fmt t =
    fpf fmt "      <TD>%d</TD>@." i;
    fpf fmt "      <TD>%s</TD>@." t.base;
    fpf fmt "      <TD>1.0</TD>@.";
    fpf fmt "      <TD>%d</TD>@." t.size;
    fpf fmt "      <TD>%s</TD>@." (String.sub t.checksum 0 8);
    fpf fmt "      <TD>%d.%0.d.%0.d</TD>@."
      (t.tm.Unix.tm_year + 1900) (t.tm.Unix.tm_mon + 1) t.tm.Unix.tm_mday;
    fpf fmt "      <TD>%d</TD>@." t.line;
    fpf fmt "      <TD>%s</TD>@." t.typ
end
module FileInfos = Set.Make(FileInfo)
module FileInfoMap = struct
  include Map.Make(String)

  open FileInfo

  let add fi m =
    let fis =
      if mem fi.path m then
        find fi.path m
      else
        FileInfos.empty
    in
    let fis = FileInfos.add fi fis in
    add fi.path fis m
end

let target_dir = ref None

let anon_fun fd =
  if Sys.is_directory fd then
    begin
      let last = String.get fd (String.length fd - 1) |> Char.escaped in
      if last = Filename.dir_sep_origin then
        target_dir := Some (String.sub fd 0 (String.length fd - 1))
      else
        target_dir := Some fd
    end
  else if Sys.file_exists fd then
    target_dir := Some fd
  else
    fpf err_formatter "[ERROR] %s is not directory@." fd

let major = "0"
let minor = "5"
let misc = "0"

let version =
  major ^ "." ^ minor ^ "." ^ misc

let usage_msg =
  "FILT Ver." ^ version ^ "\n" ^
  "main.byte <options> <target_dir>"

let ext_check = ref false
let categorizing = ref false

let out_fmt = ref None
let set_out f =
  out_fmt := Some (open_out f)

let specs = [
  "-v", Arg.Set verbose,        "    set verbose.";
  "-f", Arg.String set_out,     "    print out to a file.";
  "-t", Arg.Set ext_check,      "    test extensions"; 
  "-c", Arg.Set categorizing,   "    print out (categorized by directory)";
  "-h", Arg.Set html,           "    print out in HTML format";
  "-r", Arg.String set_root,    "    set root dir. of path (for print only)";
  "-w", Arg.String set_slash,   "    set file descripter";
]

let is_valid_extensions ?(safe=true) fd =
  try
    FileInfo.gen ~safe fd |> ignore; true
  with
  | _ -> false

module Util = struct

  let rec get_ext_files files fd =
    if Sys.is_directory fd then
      Sys.readdir fd |>
      Array.fold_left (fun files fd' ->
        match fd' with
        | ".svn" -> files
        | fd' -> get_ext_files files (fd ^ Filename.dir_sep_origin ^ fd')
      ) files
    else if Sys.file_exists fd then
      if is_valid_extensions ~safe:false fd |> not then
        begin
          if !verbose then fpf err_formatter "[ADD][F] %s@." fd;
          FileExtMap.add fd files
        end
      else
        begin
          if !verbose then fpf err_formatter "[IGNORE] %s@." fd;
          files
        end
    else
      begin
        if !verbose then fpf err_formatter "%s is not exists@." fd;
        files
      end

  let rec get_files files fd =
    if Sys.is_directory fd then
      Sys.readdir fd |>
      Array.fold_left (fun files fd' ->
        if StrSet.mem fd' !s3 then files else
        get_files files (fd ^ Filename.dir_sep_origin ^ fd')
      ) files
    else if Sys.file_exists fd then
      if is_valid_extensions fd then
        begin
          if !verbose then fpf err_formatter "[ADD][F] %s@." fd;
          FileInfos.add (FileInfo.gen fd) files
        end
      else
        begin
          if !verbose then fpf err_formatter "[IGNORE] %s@." fd;
          files
        end
    else
      begin
        if !verbose then fpf err_formatter "%s is not exists@." fd;
        files
      end

  let read_lines_of f =
    let buf = ref [] in 
    try
      let chnl = open_in f in
      while true do
        buf := input_line chnl :: !buf
      done; []
    with
    | End_of_file -> List.rev !buf

end

open Util

let run () =
  match !target_dir with
  | None -> ()
  | Some start_point ->
    fpf std_formatter "target dir : %s@." start_point;
    let f = "fileinfo.db" in
    fpf std_formatter "load file info. from %s@." f;
    let fileinfo =
      try
        let chnl = open_in f in
        let lexbuf = Lexing.from_channel chnl in
        let r = Parser.ss Lexer.token lexbuf in
        close_in chnl; r
      with
      | exn ->
        fpf err_formatter "error at line %i@." !Lexer.line;
        raise exn
    in
    let _m1, _m2 =
      List.fold_left (fun (m1, m2) (ext, v, b) ->
        let ext = String.lowercase_ascii ext in
        if b then
          StrMap.add ext v m1, m2
        else
          m1, StrMap.add ext v m2
      ) (StrMap.empty, StrMap.empty) fileinfo
    in
    m1 := _m1; m2 := _m2;
    let ignores =
      try
        fpf std_formatter "load ignorable extensions from %s@." f;
        Lexer.line := 1;
        let chnl = open_in "ignore.db" in
        let lexbuf = Lexing.from_channel chnl in
        let r = Parser.is Lexer.token lexbuf in
        close_in chnl; r
      with
      | exn ->
        fpf err_formatter "error at line %i@." !Lexer.line;
        raise exn
    in
    let ignores = ".svn" :: ".git" :: ignores in
    let _s3 =
      List.fold_left (fun ss s -> StrSet.add s ss) StrSet.empty ignores
    in
    s3 := _s3;
    if !ext_check then
      begin
        begin
        let files = get_ext_files FileExtMap.empty start_point in
        let fmt =
          match !out_fmt with
          | None -> std_formatter
          | Some chnl -> Format.formatter_of_out_channel chnl
        in
        if FileExtMap.is_empty files |> not then
          begin
            fpf fmt "* list of file(s) with unknown type in %s --- @." start_point;
            FileExtMap.iter (fun ext fs ->
              fpf fmt "[EXT = '%s']@." ext;
              Files.fold (fun f i ->
                fpf fmt "\t[%d] %s@." i f;
                i + 1
              ) fs 1 |> ignore;
            ) files;
            fpf fmt "------------------------------------ end of list * @.";
          end
        else
          begin
            fpf fmt "* no unknown files in %s@." start_point;
          end;
        match !out_fmt with
        | Some chnl -> close_out chnl
        | None -> ()
      end
      end
    else
      begin
        let files = get_files FileInfos.empty start_point in
        let num_of_files = FileInfos.cardinal files in
        let fmt =
          match !out_fmt with
          | None -> std_formatter
          | Some chnl -> Format.formatter_of_out_channel chnl
        in
        if num_of_files > 0 then
          if !html then
            begin
              fpf fmt "<!DOCTYPE html>@.";  
              fpf fmt "<HTML>@.";
              fpf fmt "  <HEAD>@.";
              fpf fmt "    <META charset='UTF-8'>@.";
              fpf fmt "  </HEAD>@.";
              fpf fmt "  <BODY>@.";
              fpf fmt "    <TABLE border='1'>@.";
              fpf fmt "      <TR>@.";
              fpf fmt "        <TH>순번</TH>@.";
              fpf fmt "        <TH>파일명</TH>@.";
              fpf fmt "        <TH>버전</TH>@.";
              fpf fmt "        <TH>크기</TH>@.";
              fpf fmt "        <TH>첵섬</TH>@.";
              fpf fmt "        <TH>생성일자</TH>@.";
              fpf fmt "        <TH>라인수</TH>@.";
              fpf fmt "        <TH>기능 설명</TH>@.";
              fpf fmt "      </TR>@.";
              FileInfos.fold FileInfoMap.add files FileInfoMap.empty |>
              FileInfoMap.iter (fun path fis ->
                fpf fmt "      <TR>@.";
                fpf fmt "        <TD colspan='8'> 저장위치 : %s</TD>@."
                  (patch_path path);
                fpf fmt "      </TR>@.";
                FileInfos.fold (fun f i ->
                  fpf fmt "      <TR>@.";
                  FileInfo.pp_html i fmt f;
                  fpf fmt "      </TR>@.";
                  i + 1
                ) fis 1 |> ignore
              );
              fpf fmt "    </TABLE>@.";
              fpf fmt "  </BODY>@.";
              fpf fmt "</HTML>@."
            end
          else if !categorizing then
            begin
              fpf fmt "* list of file(s) in %s --- @." start_point;
              FileInfos.fold FileInfoMap.add files FileInfoMap.empty |>
              FileInfoMap.iter (fun path fis ->
                fpf fmt "[%s]@." path;
                FileInfos.fold (fun f i ->
                  fpf fmt "\t[%d] %a@." i (FileInfo.pp ~nopath:true) f;
                  i + 1
                ) fis 1 |> ignore
              );
              fpf fmt "------------------------------------ end of list * @."
            end
          else
            begin
              fpf fmt "* list of [%d] file(s) in %s --- @." num_of_files start_point;
              FileInfos.fold (fun f i ->
                fpf fmt "[%d] %a@." i (FileInfo.pp ~nopath:false) f;
                i + 1
              ) files 1 |> ignore;
              fpf fmt "----------------- end of files * @."
            end
        else
          begin
            fpf fmt "* no target files in %s@." start_point;
          end;
        match !out_fmt with
        | Some chnl -> close_out chnl
        | None -> ()
      end

let _ =
  let _ = Arg.parse (Arg.align specs) anon_fun usage_msg in
  run () |> (fun _ -> if !has_error_case then exit 1 else exit 0)
