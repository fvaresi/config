{:user {:dependencies [[slamhound "1.5.5"]]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :plugins [
                  [cider/cider-nrepl "0.10.0-SNAPSHOT"]
                  [refactor-nrepl "1.2.0-SNAPSHOT"]
                  [lein-droid "0.4.0-alpha4"]
                  [lein-try "0.4.3"]
                  ]}

 :android-common {:dependencies [[cider/cider-nrepl "0.10.0-SNAPSHOT"]]
                  :android {:aot-exclude-ns ["cider.nrepl.middleware.util.java.parser"
                                             "cider.nrepl" "cider-nrepl.plugin"]
                            :sdk-path "/home/fvaresi/opt/android-sdk-linux"}}}
