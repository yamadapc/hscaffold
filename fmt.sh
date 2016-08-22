hpretty () {
        find . | grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn} "\.hs$" | while read fp
        do
                echo "hindent $fp"
                hindent --style cramer $fp
        done
        find . | grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn} "\.hs$" | while read fp
        do
                echo "stylish-haskell $fp"
                stylish-haskell -i $fp
        done
}

hpretty
