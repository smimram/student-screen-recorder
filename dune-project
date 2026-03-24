(lang dune 3.14)
(name ssr)
(source (github smimram/student-screen-recorder))
(version 0.1.0)
(license AGPL-3.0)
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "Samuel Mimram <smimram@gmail.com>")
 
(generate_opam_files true)

(package
 (name ssr)
 (synopsis "A web application to regularly record screenshots")
 (description "This is a small web application to regularly take a screenshot of the screen of students, typically to make sure that there is no cheating during an exam. Image capture is done by using capabilities of modern browsers.")
 (depends
   dream
   yaml
   csv
 )
)
