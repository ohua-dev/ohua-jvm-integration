;
; ohua : loader.clj
;
; Copyright (c) Sebastian Ertel, Justus Adam 2016. All Rights Reserved.
;
; This source code is licensed under the terms described in the associated LICENSE.TXT file.
;

(ns ohua.util.loader
  (:import (java.nio.file Files FileSystem Path FileSystemNotFoundException FileSystems)
           (java.net URL URI)))


(defn find-urls [^FileSystem fs ^String folder ^String glob]
  (let [^Path p (.getPath fs folder (into-array String []))]
    (with-open [file-stream (Files/newDirectoryStream p glob)]
      (doall (seq file-stream)))))

(defn load-from-classpath [folder glob]
  (doall
    (mapcat
      (fn [^URL url]
        (let [fs-and-file (-> url
                              (.toURI)
                              (.toString)
                              (.split "!"))
              uri (URI/create (first fs-and-file))]
          (if (= 2 (count fs-and-file))
            (find-urls

              (try (FileSystems/getFileSystem uri)
                   (catch FileSystemNotFoundException _
                     ; use the zip file system for the jar
                     (FileSystems/newFileSystem uri {})))
              (second fs-and-file)
              glob)
            (find-urls
              (FileSystems/getDefault)
              (.getPath uri)
              glob)
            )))
      (enumeration-seq (-> (Thread/currentThread)
                           (.getContextClassLoader)
                           (.getResources folder))))))
