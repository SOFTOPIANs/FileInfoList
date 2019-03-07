open Format

let (|>) a f = f a
let fpf = Format.fprintf

module File = String

module Ext = String

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

module Line = struct
  type t = int
  let compare = Pervasives.compare
end

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
    try
      let msg =
        match String.lowercase_ascii ext with
        | ".c" | ".model" -> "C source file"
        | ".cpp" | ".cc" | "cxx" -> "C++ source file"
        | ".mm" | ".m" -> "Objective-C source file"
        | ".ml" -> "OCaml source file"
        | ".mli" -> "OCaml interface file"
        | ".txt" -> "Text File"
        | ".h" -> "C header file"
        | ".hpp" -> "C++ header file"
        | ".md" -> "Markdown file"
        | ".targets" -> "target files"
        | ".bat" -> "batch file"
        | ".td" -> "LLVM target definition file"
        | ".ll" -> "LLVM IR file"
        | ".bc" -> "LLVM bitcode file"
        | ".el" -> "Lisp source file"
        | ".py" -> "Python source file"
        | ".sh" -> "shell script file"
        | ".pl" -> "Perl script file"
        | ".js" -> "JavaScript source file"
        | ".css" -> "Hypertext Cascading Style Sheet file"
        | ".applescript" -> "Apple script file"
        | ".cs" -> "C# source file"
        | ".go" -> "Go source file"
        | ".ico" -> "Icon file"
        | ".rst" -> "reStructured text file"
        | ".html" -> "HTML source file"
        | ".dox" -> "Doxygen file"
        | ".bmp" | ".png" | ".jpg" | ".jpeg" | ".gif" -> "Image file"
        | ".resx" -> "ResX Schema file"
        | ".vsct" -> "Visual Studio command table file"
        | ".sln" -> "Visual Studio solution file"
        | ".csproj" -> "C# project file"
        | ".cu" -> "CUDA source file"
        | ".json" -> "JSON file"
        | ".cl" | ".gch" | ".pch" -> "Pre compiled header file"
        | ".s" -> "assembly file"
        | ".i" -> "LLVM interpreter file"
        | ".proftext" -> "LLVM Profdata file"
        | ".prof" -> "LLVM Profdata file"
        | ".cmake" | ".guess" -> "CMake files"
        | ".vim" -> "Vim file"
        | ".1" | ".in" | ".exports" | ".config" | ".map" | ".modulemap"
        | ".test" | ".cfg" | ".def" | ".args" | ".f90" 
        | ".utf16le" | ".f95" | ".9.3" | ".3" | ".bfd" | ".gold"
        | ".syms" | ".lld-link2" | ".rs" | ".cppm" | ".result" | ".data" 
        | ".profraw" | ".fixed" | ".h-1" | ".h-0" | ".remap" | ".tbd" | ".build"
        | ".timestamp" | ".ii" | ".inc" | ".conf" | ".7" | ".dict" | ".regex"
        | ".system" | ".extra" | ".list" | ".ignore" | ".clang-tidy"
        | ".plist" | ".tmLanguage" | ".supp" | ".grm" | ".x86_64" | ".i386"
        | ".macho" | ".lst" ->
          "Misc file"
        | ".scpt" | ".dia" | ".lib" | ".v3" | ".v1" | ".v2" | ".idx"
        | ".hmap" | ".bin" | ".exe" ->
          "Misc binary file"
        | ".sphinx" -> "Make file"
        | ".rule" -> "Equivalence check rule file"
        | ".xml" -> "XML file"
        | ".yaml" -> "YAML file"
        | ".ini" -> "Configuration file"
        | ".patch" -> "Patch file"
        | ".cgi" -> "CGI file"
        | ".dll" -> "Dynamic Library file"
        | ".pbd" -> "PBD file"
        | ".obj" -> "Object file"
        | ".licx" -> "License file"
        | ".resources" -> "Image resource file"
        | ".template" -> "Assembly Info. template file"
        | ".properties" -> "Java configure file"
        | ".svg" -> "Scalable Vector Graphics image file"
        | ".ttf" -> "TrueType Font"
        | ".woff" -> "Web Open Font Format"
        | ".woff2" -> "Web Open Font Format version 2"
        | ".java" -> "Java source file"
        | "" ->
          begin
            match base with
            | "INSTALL" -> "INSTALL file"
            | ".arcconfig" -> "Arcanist config file"
            | "git-clang-format" | ".gitignore" | ".keep" | ".DS_Store" 
            | "gentoo-release" | "config-x86_64-pc-linux-gnu" 
            | "x86_64-pc-linux-gnu-4.9.3" | "x86_64-unknown-linux-gnu-as"
            | "x86_64-unknown-linux-gnu-ld" | "mipsel-linux-android-ld"
            | "aarch64-linux-android-ld" | "arm-linux-androideabi-ld"
            | "i686-linux-android-ld" | "i386-unknown-linux-gnu-ld"
            | "i386-unknown-linux-gnu-as" | "armv7-windows-itanium-ld"
            | "x86_64--linux-as" | "x86_64--linux-ld" | "Module" | "starts_a_scope" 
            | "passmgr_builder" | "test_file" | "_tags" | "aa" | "ab" | "ba" | "a" 
            | "aaa" | "aab" | "bbb" | "Linux" | "Posix" ->
              "Misc file"
            | "ld" | "prefix-ld" | "i386-unknown-linux-ld" | "as"
            | "ends_a_scope_only" | "starts_a_scope_only" | "ends_a_scope"
            | ".clang-format" | "vector" | "modmap" | "DependsOnModule"
            | "NoUmbrella" | "map" | "streambuf" | "map1" | "map2" | "map3"
            | "__config" | "type_traits" | "cstddef" | ".system_framework"
            | "typeinfo" ->
              "Misc file"
            | "README" | "readme" | ".LLVM" ->
              "Text file"
            | "Dockerfile" -> "Docker file"
            (* special files without extensions *)
            | "remapped-file-3" | "remapped-file-2" | "remapped-file" ->
              "C source file"
            | "warn-inconsistent-missing-destructor-override" ->
              "C++ source file"
            | "scan-build" | "ccc-analyzer" | "c++-analyzer" -> 
              "Perl script file"
            | "set-xcode-analyzer" | "scan-view" | "intercept-build" 
            | "analyze-build" | "intercept-cc" | "intercept-c++"
            | "analyze-cc" | "analyze-c++" ->
              "Python source file"
            | "Makefile" | "makefile" -> "Make file"
            | "configure" -> "Configure file"
            | _ ->
              raise (UnknownFile base)
          end
        | _ ->
          raise (UnknownExt (base, ext))
      in
      Known ("\'" ^ msg ^ "\'")
    with
    | UnknownFile (f) -> UnkF
    | UnknownExt (f, ext) -> UnkE

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
      | UnkE | UnkF -> if safe then "Misc File" else raise (UnknownFile filename)
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
      if last = Filename.dir_sep then
        target_dir := Some (String.sub fd 0 (String.length fd - 1))
      else
        target_dir := Some fd
    end
  else if Sys.file_exists fd then
    target_dir := Some fd
  else
    fprintf err_formatter "[ERROR] %s is not directory@." fd

let major = "0"
let minor = "3"
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
        | fd' -> get_ext_files files (fd ^ Filename.dir_sep ^ fd')
      ) files
    else if Sys.file_exists fd then
      if is_valid_extensions ~safe:false fd |> not then
        begin
          if !verbose then fprintf err_formatter "[ADD][F] %s@." fd;
          FileExtMap.add fd files
        end
      else
        begin
          if !verbose then fprintf err_formatter "[IGNORE] %s@." fd;
          files
        end
    else
      begin
        if !verbose then fprintf err_formatter "%s is not exists@." fd;
        files
      end

  let rec get_files files fd =
    if Sys.is_directory fd then
      Sys.readdir fd |>
      Array.fold_left (fun files fd' ->
        match fd' with
        | ".svn" -> files
        | fd' -> get_files files (fd ^ Filename.dir_sep ^ fd')
      ) files
    else if Sys.file_exists fd then
      if is_valid_extensions fd then
        begin
          if !verbose then fprintf err_formatter "[ADD][F] %s@." fd;
          FileInfos.add (FileInfo.gen fd) files
        end
      else
        begin
          if !verbose then fprintf err_formatter "[IGNORE] %s@." fd;
          files
        end
    else
      begin
        if !verbose then fprintf err_formatter "%s is not exists@." fd;
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
    fprintf std_formatter "target dir : %s@." start_point;
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
            fprintf fmt "* list of file(s) with unknown type in %s --- @." start_point;
            FileExtMap.iter (fun ext fs ->
              fprintf fmt "[EXT = '%s']@." ext;
              Files.fold (fun f i ->
                fprintf fmt "\t[%d] %s@." i f;
                i + 1
              ) fs 1 |> ignore;
            ) files;
            fprintf fmt "------------------------------------ end of list * @.";
          end
        else
          begin
            fprintf fmt "* no unknown files in %s@." start_point;
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
          if !categorizing then
            begin
              fprintf fmt "* list of file(s) in %s --- @." start_point;
              FileInfos.fold FileInfoMap.add files FileInfoMap.empty |>
              FileInfoMap.iter (fun path fis ->
                fprintf fmt "[%s]@." path;
                FileInfos.fold (fun f i ->
                  fprintf fmt "\t[%d] %a@." i (FileInfo.pp ~nopath:true) f;
                  i + 1
                ) fis 1 |> ignore
              );
              fprintf fmt "------------------------------------ end of list * @."
            end
          else
            begin
              fprintf fmt "* list of [%d] file(s) in %s --- @." num_of_files start_point;
              FileInfos.fold (fun f i ->
                fprintf fmt "[%d] %a@." i (FileInfo.pp ~nopath:false) f;
                i + 1
              ) files 1 |> ignore;
              fprintf fmt "----------------- end of files * @."
            end
        else
          begin
            fprintf fmt "* no target files in %s@." start_point;
          end;
        match !out_fmt with
        | Some chnl -> close_out chnl
        | None -> ()
      end

let _ =
  let _ = Arg.parse (Arg.align specs) anon_fun usage_msg in
  run () |> (fun _ -> if !has_error_case then exit 1 else exit 0)
