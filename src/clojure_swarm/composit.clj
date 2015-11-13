(ns clojure-swarm.composit
  (:require [clojure.data :refer [diff] :as data]))

(comment
  "Component lyfecycle definition"
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
                    :reference (atom component)}})

(defn construct-component [definition]
  (let [component-name (:name definition)
        constructor (resolve (:constructor definition))]
    {component-name {:lifecycle (atom (dissoc definition :name))
                     :reference (atom (constructor))
                     :dependencies (atom {})
                     :configuration (atom {})}}))

(defn construct-system [definitions]
  (into {} (merge (map construct-component definitions))))

(defn- assoc-injection-component [system injection name binding]
  (let [dependency (-> system name :reference)]
    (if dependency
      (assoc injection binding dependency)
      injection)))

(defn inject-component-dependencies! [system component dependencies]
  (locking component
    (let [injection (reduce-kv #(assoc-injection-component system %1 %2 %3) {} dependencies)
          reference (:reference component)
          curr-dependencies (:dependencies component)]
      (swap! reference #(into % injection))
      (reset! curr-dependencies dependencies)
      component)))

(defn inject-system-dependencies! [system dependencies]
  (locking system
    (doseq [[name component] system]
      (when-let [component-dependencies (name dependencies)]
        (inject-component-dependencies! system component component-dependencies)))
    system))

(defn configure-component! [component configuration]
  (locking component
    (let [configurator (-> component
                           :lifecycle
                           deref
                           :configurator
                           resolve)
          reference (:reference component)
          curr-configuration (:configuration component)]
      (swap! reference #(configurator % configuration))
      (reset! curr-configuration configuration)
      component)))

(defn configure-system! [system configuration]
  (locking system
    (doseq [[name component] system]
      (when-let [upd-configuration (name configuration)]
        (let [curr-configuration @(:configuration component)
              diff-configuration (data/diff curr-configuration upd-configuration)]
          (when (or
                  (first diff-configuration)
                  (second diff-configuration))
            (configure-component! component upd-configuration)))))
    system))

(defn cleanup-component! [component]
  (locking component
    (when-let [cleaner (-> component
                           :lifecycle
                           deref
                           :cleaner
                           resolve)]
      (let [reference (:reference component)]
        (swap! reference cleaner)))
    component))

(defn cleanup-system! [system]
  (locking system
    (doseq [[_ component] system]
      (cleanup-component! component))
    system))
