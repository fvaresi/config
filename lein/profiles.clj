{:user {:dependencies []
        :plugins [[cider/cider-nrepl "0.10.0-SNAPSHOT"]
                  [refactor-nrepl "1.2.0-SNAPSHOT"]
                  [lein-droid "0.4.0-alpha4"]]}

 :android-common {:dependencies [[cider/cider-nrepl "0.10.0-SNAPSHOT"]]
                  :android {:aot-exclude-ns ["cider.nrepl.middleware.util.java.parser"
                                             "cider.nrepl" "cider-nrepl.plugin"]
                            :sdk-path "/home/fvaresi/opt/android-sdk-linux"}}}
