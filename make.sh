#! /usr/bin/env racket
#lang racket

(require file/zip)
(require json)
(require "visualizer/visual.rkt")
(provide (all-from-out "visualizer/visual.rkt"))

(define modFolder "/mnt/gallery/gameFiles/factorio/mods/")
(define serverModFolder "/home/veden/Downloads/factorio/mods/")
(define packageName '())
(define configuration '())

(define (makeZip folder)
  (clear)

  (zip (string-append folder
                      packageName
                      ".zip")
       #:path-prefix packageName
       (string->path "info.json")
       (string->path "control.lua")
       (string->path "data.lua")
       (string->path "data-updates.lua")
       (string->path "data-final-fixes.lua")
       (string->path "COPYING")
       (string->path "tests.lua")
       (string->path "changelog.txt")
       (string->path "settings.lua")
       (string->path "README.md")
       (string->path "thumbnail.png")
       (string->path "NOTICE")
       (string->path "libs")
       (string->path "sounds")
       (string->path "migrations")
       (string->path "locale")
       (string->path "graphics")
       (string->path "prototypes")))

(define (copyFile fileName modFolder)
  (copy-file (string->path fileName)
             (string->path (string-append modFolder
                                          packageName
                                          "/"
                                          fileName))))

(define (copyDirectory directoryName modFolder)
  (copy-directory/files (string->path directoryName)
                        (string->path (string-append modFolder
                                                     packageName
                                                     "/"
                                                     directoryName))))

(define (copyFiles modFolder)
  (let ((packagePath (string->path (string-append modFolder
                                                  packageName))))
    (when (directory-exists? packagePath)
      (delete-directory/files packagePath))
    (sleep 0.1)
    (make-directory packagePath)
    (copyFile "control.lua" modFolder)
    (copyFile "info.json" modFolder)
    (copyFile "data.lua" modFolder)
    (copyFile "COPYING" modFolder)
    (copyFile "NOTICE" modFolder)
    (copyFile "README.md" modFolder)
    (copyFile "data-updates.lua" modFolder)
    (copyFile "data-final-fixes.lua" modFolder)
    (copyFile "settings.lua" modFolder)
    (copyFile "changelog.txt" modFolder)
    (copyFile "tests.lua" modFolder)
    (copyFile "thumbnail.png" modFolder)
    (copyDirectory "libs" modFolder)
    (copyDirectory "locale" modFolder)
    (copyDirectory "migrations" modFolder)
    (copyDirectory "sounds" modFolder)
    (copyDirectory "graphics" modFolder)
    (copyDirectory "prototypes" modFolder)))

(define (set-metadata)
  (set! configuration (call-with-input-file "info.json"
                        (lambda (port)
                          (string->jsexpr (port->string port)))))
  (set! packageName (string-append (string-replace (hash-ref configuration 'name) " " "_")
                                   "_"
                                   (hash-ref configuration 'version))))

(define (copy)
  (set-metadata)
  (copyFiles modFolder)
  (print (string-append "copying " (hash-ref configuration 'name) (hash-ref configuration 'version))))

(define (zipIt)
  (set-metadata)
  (makeZip modFolder)
  (print (string-append "zipping " (hash-ref configuration 'name) (hash-ref configuration 'version))))

(define (clear)
  (set-metadata)
  (let ((packagePath (string->path (string-append modFolder
                                                  packageName
                                                  ".zip")))
        (unzippedPath (string->path (string-append modFolder
                                                   packageName))))

    (when (directory-exists? unzippedPath)
      (delete-directory/files unzippedPath))

    (when (file-exists? packagePath)
      (delete-file packagePath)))
  (print (string-append "clearing " (hash-ref configuration 'name) (hash-ref configuration 'version))))

(let ((arg (vector-ref (current-command-line-arguments) 0)))
  (cond ((equal? arg "copy") (copy))
        ((equal? arg "zip") (zipIt))
        ((equal? arg "clear") (clear))
        ((equal? arg "visualize") (visualize))
        (else "Invalid command arg [copy,zip,clear,visualize]")))
