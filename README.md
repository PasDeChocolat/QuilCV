QuilCV
======

Some [Clojure](http://clojure.org) [Quil](https://github.com/quil/quil) + [OpenCV](http://opencv.org) demo projects

### Prerequisites
- Working Clojure install
- Some familiarity with Quil (Clojure's Processing port)
- Some familiarity / interest in OpenCV
- OpenCV JARs installed in Maven (see below)

### The Demos

The demos highlighted here are Quil sketches. Run them as you would any Quil sketch. I prefer to eval the ns in an Emacs Cider REPL.

#### Lucas-Kanade Optical Flow

![Flow Vectors](https://raw.githubusercontent.com/PasDeChocolat/QuilCV/master/OpticalFlow/LucasKanade/pics/optical-flow-vectors.png "Flow Vectors")
Optical Flow: Vectors

![Flow Corners](https://raw.githubusercontent.com/PasDeChocolat/QuilCV/master/OpticalFlow/LucasKanade/pics/optical-flow-corners.png "Flow Corners")
Optical Flow: Corners

- [Flow vectors drawn as lines](https://github.com/PasDeChocolat/QuilCV/blob/master/OpticalFlow/LucasKanade/src/videotest/flow_only.clj)
- [Corners highlighted](https://github.com/PasDeChocolat/QuilCV/blob/master/OpticalFlow/LucasKanade/src/videotest/optical_flow_crit.clj)
- [Corners highlighted, variable camera size](https://github.com/PasDeChocolat/QuilCV/blob/master/OpticalFlow/LucasKanade/src/videotest/opti_flow_vari_cam.clj)
- [Larger flow vectors example, used as input to gesture demo](https://github.com/PasDeChocolat/QuilCV/blob/master/OpticalFlow/LucasKanade/src/videotest/basic_mover/core.clj)
- [Flocking + flow as gestures, with bad performance](https://github.com/PasDeChocolat/QuilCV/blob/master/OpticalFlow/LucasKanade/src/videotest/basic_mover/mover_core.clj)

### OpenCV Required

You'll have to build and install the OpenCV JAR files for your setup. Installation tested on Archlinux and OS X. The following is a rough summary of "[Introduction to OpenCV Development with Clojure](http://docs.opencv.org/doc/tutorials/introduction/clojure_dev_intro/clojure_dev_intro.html#clojure-dev-intro)."

Note, at time of writing, the latest version was 2.4.10. So, you may need to replace `2410` in the instructions below with the current version.

#### Java
Confirm Java is already installed:
$ java -version
java version "1.8.0_11"
Java(TM) SE Runtime Environment (build 1.8.0_11-b12)
Java HotSpot(TM) 64-Bit Server VM (build 25.11-b03, mixed mode)

Otherwise, install it.

#### On OS X, I used Homebrew, requiring the following

```` bash
brew update
brew install cmake
brew python2
````

If python2 not available, make sure you have Python pointing to Python v2.

#### Build the OpenCV JAR files

```` bash
git clone https://github.com/Itseez/opencv.git
cd opencv
git checkout 2.4
mkdir build
cd build
cmake -DBUILD_SHARED_LIBS=OFF ..
... output omitted ...
make -j8
````

NO NEED TO INSTALL IT, WE JUST NEED THE JAR. So, there's no reason to install it.

Built files are:
```` bash
build/bin/     
build/lib/libopencv_java2410.dylib
````

#### Use lein-localrepo to install JAR files

Create a file named profiles.clj in the ~/.lein directory and copy into it the following content:
```` clojure
{:user {:plugins [[lein-localrepo "0.5.3"]]}}
````

You may need to change `0.5.3` to the latest version of `lein-localrepo`.

```` bash
mkdir clj-opencv
cd clj-opencv
cp <path>/<to>/opencv/build/bin/opencv-2410.jar .
mkdir -p native/macosx/x86_64
cp ../opencv/build/lib/libopencv_java2410.dylib native/macosx/x86_64
jar -cMf opencv-native-2410.jar native
lein localrepo install opencv-2410.jar opencv/opencv 2.4.10
lein localrepo install opencv-native-2410.jar opencv/opencv-native 2.4.10
````