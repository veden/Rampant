(module BuildScript racket
  (provide run)
  
  (require file/zip)
  (require json)
  
  (define modFolder "/home/veden/.factorio/mods/")
  (define zipModFolder "/data/games/factorio/mods/")
  (define configuration (call-with-input-file "info.json"
                          (lambda (port)
                            (string->jsexpr (port->string port)))))
  (define packageName (string-append (string-replace (hash-ref configuration 'name) " " "_")
                                     "_" 
                                     (hash-ref configuration 'version)))
  
  (define (makeZip folder)
    (let ((packagePath (string->path (string-append folder
                                                    packageName
                                                    ".zip"))))
      (when (file-exists? packagePath)
        (delete-file packagePath)))
    (zip (string-append folder
                        packageName 
                        ".zip")
         #:path-prefix packageName
         (string->path "info.json")
         (string->path "control.lua")
         (string->path "config.lua")
         (string->path "data.lua")
         (string->path "data-updates.lua")
         (string->path "data-final-fixes.lua")
         (string->path "LICENSE.md")
         (string->path "tests.lua")
         (string->path "Upgrade.lua")
         (string->path "settings.lua")
         (string->path "README.md")
         (string->path "NOTICE")
         (string->path "libs")
         (string->path "sounds")
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
      (copyFile "config.lua" modFolder)
      (copyFile "info.json" modFolder)
      (copyFile "data.lua" modFolder)
      (copyFile "data-updates.lua" modFolder)
      (copyFile "data-final-fixes.lua" modFolder)
      (copyFile "settings.lua" modFolder)
      (copyFile "Upgrade.lua" modFolder)
      (copyFile "tests.lua" modFolder)
      (copyDirectory "libs" modFolder)
      (copyDirectory "locale" modFolder)
      (copyDirectory "sounds" modFolder)
      (copyDirectory "graphics" modFolder)
      (copyDirectory "prototypes" modFolder)))
  
  (define (run)
    (copyFiles modFolder)
    ;;(copyFiles zipModFolder)
    ;;(makeZip modFolder)
    )
  )
