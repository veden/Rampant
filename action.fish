#!/bin/fish

argparse --min-args=1 --max-args=1 'dir=?' 'serverDir=?' 'silent' 'auth=?' 'help' -- $argv
or return 2

if test $_flag_help
    echo "commands are:
    copy: --dir, --serverDir
    zip: --dir, --serverDir, --silent
    upload: --dir, --auth
    options:
    --dir=<can set directory for output or file to upload>
    --serverDir=<can set secondary directory for output when testing multiplayer>
    --auth=<api key for uploading to mod portal>
    --help=<get commands and options printed>"
    return 0
end

function copyFiles --argument-names copyFolder
    mkdir -p $copyFolder

    cp --verbose ./*.lua $copyFolder
    cp --verbose ./*.png $copyFolder
    cp --verbose ./*.json $copyFolder
    cp --verbose -r ./sounds $copyFolder
    cp --verbose -r ./locale $copyFolder
    cp --verbose -r ./libs $copyFolder
    cp --verbose -r ./graphics $copyFolder
    cp --verbose -r ./prototypes $copyFolder
    cp --verbose -r ./migrations $copyFolder
    cp --verbose ./COPYING $copyFolder
    cp --verbose ./changelog.txt $copyFolder
    cp --verbose ./README.md $copyFolder
end

set modFolder $_flag_dir
if test -z "$modFolder"
    set modFolder "/mnt/gallery/gameFiles/factorio/mods"
end

set modName (jq -r .name info.json)
set modVersion (jq -r .version info.json)
set title $modName"_"$modVersion
set zipName "$modFolder/$title.zip"
set modPath "$modFolder/$title"

switch $argv[1]
    case copy
        echo "copying"
        rm -f $zipName

        copyFiles $modPath

        if test -n "$_flag_serverDir"
            copyFiles $_flag_serverDir
        end
    case zip
        if test -z "$_flag_silent"
            echo "zipping"
        end
        rm -rf $modPath
        rm -f $zipName
        ln -s (pwd) $title

        set zipProgress (zip $zipName \
            $title/*.lua $title/*.png $title/*.json $title/sounds/**/* $title/locale/**/* \
            $title/libs/**/* $title/graphics/**/* $title/prototypes/**/* $title/migrations/**/* \
            $title/COPYING $title/changelog.txt $title/README.md)

        if test -z "$_flag_silent"
            echo $zipProgress
        end

        if test -n "$_flag_serverDir"
            cp $zipName $_flag_serverDir
        end

        rm ./$title

        echo $zipName

    case upload
        echo "init uploading"

        set initResponse (curl --no-progress-meter -X POST "https://mods.factorio.com/api/v2/mods/releases/init_upload" \
            -H "Authorization: Bearer $_flag_auth" -H "Content-Type: multipart/form-data" \
            -F mod=$modName)

        if test (echo "$initResponse" | jq -r .error '-') != "null"
            echo "init upload failed: $initResponse"
            return 3
        end

        echo "uploading"

        set finishResponse (curl --no-progress-meter -X POST (echo "$initResponse" | jq -r .upload_url '-') \
            -H "Authorization: Bearer $_flag_auth" -H "Content-Type: multipart/form-data" \
            -F file="@$_flag_dir")

        if test (echo "$finishResponse" | jq -r .error '-') != "null"
            echo "finish upload failed: $finishResponse"
            return 4
        else
            echo "upload success: $finishResponse"
        end

    case '*'
        echo "commands are: copy, zip"
        return 1
end
