# File Infomation Listing Tool v0.7.1 (20190508)

### required opam package

 - OCaml version >= 4.04
 - oasis
 
### How to build

 - install opam (by brew : `brew install opam`)
 - install oasis package (`opam install oasis`)
 - run `oasis setup`
 - run `make`
 - get the binary file : `main.byte` or `main.native`
 
### How to run

 - `./main.{byte|native} -t <dir_name>` : search every files in <dir_name>, listing files of unknown extension (categorized by extension).
 - `./main.{byte|native} <dir_name>` : search every files in <dir_name>, listring informations of files.

### Addtional options

 - `./main.{byte|native} ... -f <output_file>` : print out to <output_file>. (default : stdout)
 - `./main.{byte|native} ... -c <output_file>` : print out (categorized by directory, not for -t mode)
 - `./main.{byte|native} ... -h` : print out to html form
 - `./main.{byte|native} ... -h2` : print out to html form (another format)
 - `./main.{byte|native} ... -r` : set root directory of path
 - `./main.{byte|native} ... -w` : set filedescripter
 
### TODO

 - move hard-coded file type list to external file [DONE]
 -- use fileinfo.db and ignore.db
 - fix bugs
