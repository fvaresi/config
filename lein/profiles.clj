{:user {:dependencies [[slamhound "1.5.5"]]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :plugins [
                  [cider/cider-nrepl "0.9.0-SNAPSHOT"]
                  [lein-droid "0.4.0-alpha2"]
                  [lein-try "0.4.3"]
                  ]}

 :android-common {:android {:sdk-path "/home/fvaresi/opt/android-sdk-linux"}}}
