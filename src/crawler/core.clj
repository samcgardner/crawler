(ns crawler.core
  (:gen-class)
  (:require [org.httpkit.client :as client])
  (:require [hickory.core :as hickory])
  (:require [hickory.select :as select])
  (:require [cemerick.url :as liburl])
  (:require [clojure.core.async :as async])
  (:require [clojure.data.json :as json]))

(declare build-sitemap)
(declare apply-response-to-state)
(declare get-links-in-page)
(declare extract-static-assets)
(declare extract-hrefs)
(declare same-origin?)
(declare select-values)
(declare build-url-string)

(def sitemap (atom {}))

(defn -main
  [& args]
  (let [url (first args)
        chan (async/chan (async/buffer 100))
        timeout-chan (async/timeout 5000)]
       (async/>!! chan url)
       (let [result (build-sitemap chan timeout-chan)]
         (json/pprint result))))

(defn build-sitemap
  [chan timeout-chan]
  (loop []
        (async/alt!!
             timeout-chan @sitemap
             chan ([url] (client/get url
                           {:async? true}
                           (fn [response] (apply-response-to-state response url chan))
                           (fn [raise] (println raise)))
                    (recur)))))

(defn apply-response-to-state
  [response url chan]
  (let [host (get (liburl/url url) :host)
        links (filter #(same-origin? host %1) (get-links-in-page response))]
    (let [filtered (filter #(not (contains? @sitemap %1)) links)]
      (swap! sitemap merge (zipmap filtered (repeat [])))
      (swap! sitemap assoc url (extract-static-assets response))
      (doall (map #(async/>!! chan %1) filtered)))))

(defn get-links-in-page
     [page]
     (let [hrefs (extract-hrefs page)]
       ;; Filter out anything that isn't an absolute http link. Technically this breaks relative URLs, but
       ;; handling them properly is hard and handling them badly seems undesirable
       (let [absolute-urls (filter #(clojure.string/starts-with? %1 "h") (remove nil? hrefs))]
           (map #(-> %1 liburl/url (select-keys [:protocol :host :path]) build-url-string) absolute-urls))))

(defn extract-static-assets
  [response]
  (let [tags (-> response :body hickory/parse hickory/as-hickory)]
    (let [scripts (map
                    #(-> %1 :attrs :src)
                    (select/select (select/and (select/tag :script) (select/attr :src)) tags))
          images (map
                   #(-> %1 :attrs :src)
                   (select/select (select/tag :img) tags))
          stylesheet (map
                       #(-> %1 :attrs :href)
                       (select/select (select/and (select/tag :link) (select/attr :type #(.equals %1 "text/css"))) tags))]
      (zipmap [:scripts :images :styles] (remove nil? [scripts images stylesheet])))))

(defn extract-hrefs
  [page]
  (let [tags (-> page :body hickory/parse hickory/as-hickory)]
    (let [anchors (select/select (select/tag :a) tags)]
      (map #(-> %1 :attrs :href) anchors))))

(defn same-origin?
  [hostname link]
  (= hostname (:host (liburl/url link))))

(defn build-url-string [urlmap]
  (clojure.string/join [(urlmap :protocol) "://" (urlmap :host) (urlmap :path)]))
