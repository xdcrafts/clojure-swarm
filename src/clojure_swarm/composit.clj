(ns clojure-swarm.composit
  (:require [clojure.data :refer [diff] :as data]))

(comment
  "Component bundle lyfecycle definition"
  [{:name :component-name
    :constructor 'component-ns/constructor-fn
    :configurator 'component-ns/configurator-fn
    :cleaner 'component-ns/cleaner-fn}])

(comment
  "Component dependencies"
  {:component-name {:dependency-name :dependant-component-name}})

(comment
  "Configuration"
  {:component-name-1 {}
   :component-name-2 {}})

(comment
  "System"
  {:component-name {:lifecycle (atom {})
                    :dependencies (atom {})
                    :configuration (atom {})
                    :component (atom component)}})

(defn construct-bundle [definition]
  (let [component-name (:name definition)
        constructor (resolve (:constructor definition))]
    {component-name {:lifecycle (atom (dissoc definition :name))
                     :component (atom (constructor))
                     :dependencies (atom {})
                     :configuration (atom {})}}))

(defn construct-system [definitions]
  (into {} (merge (map construct-bundle definitions))))

(defn- assoc-injection-component [system injection name binding]
  (let [dependency (-> system name :component)]
    (if dependency
      (assoc injection binding dependency)
      injection)))

(defn inject-bundle-dependencies! [system bundle dependencies]
  (locking bundle
    (let [injection (reduce-kv #(assoc-injection-component system %1 %2 %3) {} dependencies)
          reference (:component bundle)
          curr-dependencies (:dependencies bundle)]
      (swap! reference #(into % injection))
      (reset! curr-dependencies dependencies)
      bundle)))

(defn inject-system-dependencies! [system dependencies]
  (locking system
    (doseq [[name bundle] system]
      (when-let [component-dependencies (name dependencies)]
        (inject-bundle-dependencies! system bundle component-dependencies)))
    system))

(defn configure-bundle! [bundle configuration]
  (locking bundle
    (let [configurator (-> bundle
                           :lifecycle
                           deref
                           :configurator
                           resolve)
          reference (:component bundle)
          curr-configuration (:configuration bundle)]
      (swap! reference #(configurator % configuration))
      (reset! curr-configuration configuration)
      bundle)))

(defn configure-system! [system configuration]
  (locking system
    (doseq [[name bundle] system]
      (when-let [upd-configuration (name configuration)]
        (let [curr-configuration @(:configuration bundle)
              diff-configuration (data/diff curr-configuration upd-configuration)]
          (when (or
                  (first diff-configuration)
                  (second diff-configuration))
            (configure-bundle! bundle upd-configuration)))))
    system))

(defn cleanup-bundle! [bundle]
  (locking bundle
    (when-let [cleaner (-> bundle
                           :lifecycle
                           deref
                           :cleaner
                           resolve)]
      (let [reference (:component bundle)]
        (swap! reference cleaner)))
    bundle))

(defn cleanup-system! [system]
  (locking system
    (doseq [[_ bundle] system]
      (cleanup-bundle! bundle))
    system))
