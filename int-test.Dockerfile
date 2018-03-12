FROM clojure:lein-2.8.1-onbuild

CMD ["lein", "test", ":only", "og.clj-dstream.integration-test"]