(defproject videotest "0.1.0-SNAPSHOT"
  :description "Quil + OpenCV demo projects"
  :url "https://github.com/PasDeChocolat/QuilCV"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [quil "2.2.2"]
                 [opencv/opencv "2.4.10"]
                 [opencv/opencv-native "2.4.10"]

                 ;; required by native-vector
                 [org.clojure/math.numeric-tower "0.0.4"]
                 ]
  :jvm-opts ["-Xmx2G" "-Xms2G"]
  :profiles {:dev {:source-paths ["dev"]}}
  :injections [(clojure.lang.RT/loadLibrary org.opencv.core.Core/NATIVE_LIBRARY_NAME)])
