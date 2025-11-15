(lang dune 3.9)
(name ssr)
(source (github smimram/student-screen-recorder))
(version 0.1.0)
(license GPL-3.0)
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "Samuel Mimram <smimram@gmail.com>")
 
(generate_opam_files true)

(package
 (name ssr)
 (synopsis "A web application to regularly record screenshots")
 (description "A web application to regularly record screenshots.")
 (depends
   dream
   yaml
 )
)
