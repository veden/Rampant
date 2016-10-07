(module BuildScript racket
    
    (require file/zip)
    (require json)
        
    (define modFolder "/data/games/factorio13.201/mods/")
    (define zipModFolder "/data/games/factorio13.202/mods/")
    ;; (define modFolder "/home/veden/.factorio/mods/")
    ;; (define zipModFolder "/data/games/factorio14.13/mods/")
    (define configuration (call-with-input-file "info.json"
                            (lambda (port)
                              (string->jsexpr (port->string port)))))
    (define packageName (string-append (string-replace (hash-ref configuration 'name) " " "_")
                                       "_" 
                                       (hash-ref configuration 'version)))
    
    (define (makeZip)
      (let ((packagePath (string->path (string-append modFolder
                                                      packageName
                                                      ".zip"))))
        (when (file-exists? packagePath)
          (delete-file packagePath)))
      (zip (string-append modFolder
                          packageName 
                          ".zip")
           #:path-prefix packageName
           (string->path "info.json")
           (string->path "control.lua")
           (string->path "config.lua")
           (string->path "data.lua")
           (string->path "LICENSE.md")
           (string->path "tests.lua")
           ;             (string->path "setupUtils.lua")
           (string->path "README.md")
           ; (string->path "setup.lua")
           (string->path "NOTICE")
           (string->path "libs")
           (string->path "locale")
           (string->path "graphics")
           (string->path "prototypes")))
    
    
    ;(current-directory "..")
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
        ;           (copyFile "setupUtils.lua" modFolder)
        (copyFile "data.lua" modFolder)
        (copyFile "tests.lua" modFolder)
        (copyDirectory "libs" modFolder)
        (copyDirectory "locale" modFolder)
        (copyDirectory "graphics" modFolder)
        (copyDirectory "prototypes" modFolder)))
    
    ;; (copyFiles modFolder)
    ;; (copyFiles zipModFolder)
    (makeZip)
    )
