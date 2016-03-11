(ns rosco.ring-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [rosco.ring :as ring]
            [ring.middleware.params :refer [wrap-params]]))


(defn- foo []
  "FOO!")

(defn foo-handler
  [request]
  {:status 200 :body (foo)})

(defn wrap-bar
  [handler]
  (fn [request]
    (let [response (handler request)]
      (update-in response [:body] #(str % " BAR")))))

(def handler
  (-> foo-handler
      wrap-bar
      (ring/wrap-exclusive-tracing [#"rosco.ring-test.foo.*"])
      wrap-params))


(def get-trace-json
  (-> ring/get-trace
      wrap-params))

(defn- read-trace [s]
  (binding [*data-readers* (assoc *data-readers* 'object identity)]
    (read-string s)))

(deftest test-ring-tracing
  (let [resp (handler (mock/request :get "/foo"))]
    (is (nil? (get-in resp [:headers "x-rosco-trace"]))))
  (let [resp     (handler (mock/request :get "/foo?trace=true"))
        trace-id (get-in resp [:headers "x-rosco-trace"])]
    (is trace-id)
    (let [resp (get-trace-json (mock/request :get (str "/?trace-id=" trace-id)))]
      (is (= 200 (:status resp)))
      (let [trace (read-trace (:body resp))]
        (is trace)))))
