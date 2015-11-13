(ns clojure-swarm.composit-test
  (:require [clojure.test :as t]
            [clojure-swarm.composit :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn service-constructor [] {:enabled? true})

(defn service-configurator [component configuration]
  (into component configuration))

(defn service-cleaner [component]
  {:enabled? false})

(def service-lifecyle-definition
  {:name :service
   :constructor 'clojure-swarm.composit-test/service-constructor
   :configurator 'clojure-swarm.composit-test/service-configurator
   :cleaner 'clojure-swarm.composit-test/service-cleaner})

(def service-dependecies
  {:client :discoverer})

(def service-configuration
  {:timeout [100 :ms]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn client-constructor [] {:enabled? true})

(defn client-configurator [component configuration]
  (into component configuration))

(defn client-cleaner [component]
  {:enabled? false})

(def client-lifecyle-definition
  {:name :client
   :constructor 'clojure-swarm.composit-test/client-constructor
   :configurator 'clojure-swarm.composit-test/client-configurator
   :cleaner 'clojure-swarm.composit-test/client-cleaner})

(def client-configuration
  {:zookeeper-connection "http://localhost:1234"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest construct-bundle-test
  (t/testing "Construction of component"
    (let [bundle (construct-bundle service-lifecyle-definition)
          lifecycle @(get-in bundle [:service :lifecycle])
          reference @(get-in bundle [:service :component])
          dependencies @(get-in bundle [:service :dependencies])
          configuration @(get-in bundle [:service :configuration])]
      (t/is
        (and lifecycle reference dependencies configuration)
        "Bundle should have lifecycle, reference, dependencies and configuration referencies")
      (t/is (=
            {:constructor 'clojure-swarm.composit-test/service-constructor
             :configurator 'clojure-swarm.composit-test/service-configurator
             :cleaner 'clojure-swarm.composit-test/service-cleaner}
            lifecycle)
            "Bundle should have expected lifecycle")
      (t/is (=
            {:enabled? true}
            reference)
            "Component should be constructed properly"))))

(t/deftest construct-system-test
  (t/testing "Construction of system"
    (let [system (construct-system [service-lifecyle-definition client-lifecyle-definition])]
      (t/is (:service system) "System should have service bundle")
      (t/is (:client system) "System should have client bundle"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest inject-bundle-dependencies-test
  (t/testing "Injection of bundle dependecies"
    (let [system (construct-system [service-lifecyle-definition client-lifecyle-definition])
          service-bundle (:service system)
          _ (inject-bundle-dependencies! system service-bundle service-dependecies)
          dependencies @(:dependencies service-bundle)
          service @(:component service-bundle)]
      (t/is (= service-dependecies dependencies) "Dependencies should be set.")
      (t/is (:discoverer service) "Service should have discoverer reference"))))

(t/deftest inject-system-dependencies-test
  (t/testing "Injection of system dependencies"
    (let [system (construct-system [service-lifecyle-definition client-lifecyle-definition])
          _ (inject-system-dependencies! system {:service service-dependecies})
          service-bundle (:service system)
          dependencies @(:dependencies service-bundle)
          service @(:component service-bundle)]
      (t/is (= service-dependecies dependencies) "Dependencies should be set.")
      (t/is (:discoverer service) "Service should have discoverer reference"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest configure-bundle-test
  (t/testing "Configuration of bundle"
    (let [bundle (:service (construct-bundle service-lifecyle-definition))
          _ (configure-bundle! bundle service-configuration)
          curr-configuration @(:configuration bundle)
          reference @(:component bundle)]
      (t/is (:timeout reference) "Service should have timeout key.")
      (t/is (= service-configuration curr-configuration) "Configuration should be set."))))

(t/deftest configure-system-test
  (t/testing "Configuration of system"
    (let [system (construct-system [service-lifecyle-definition client-lifecyle-definition])
          _ (configure-system! system {:service service-configuration})
          bundle (:service system)
          curr-configuration @(:configuration bundle)
          reference @(:component bundle)]
      (t/is (:timeout reference) "Service should have timeout key.")
      (t/is (= service-configuration curr-configuration) "Configuration should be set."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest cleanup-bundle-test
  (t/testing "Component cleanup"
    (let [bundle (:service (construct-bundle service-lifecyle-definition))
          _ (cleanup-bundle! bundle)
          enabled? (:enabled? @(:component bundle))]
      (t/is (not enabled?) "Service must be disabled."))))

(t/deftest cleanup-component-test
  (t/testing "Component cleanup"
    (let [system (construct-system [service-lifecyle-definition client-lifecyle-definition])
          _ (cleanup-system! system)
          service-enabled? (:enabled? @(get-in system [:service :component]))
          client-enabled? (:enabled? @(get-in system [:client :component]))]
      (t/is (not service-enabled?) "Service must be disabled.")
      (t/is (not client-enabled?) "Client must be disabled."))))
