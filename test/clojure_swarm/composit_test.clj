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

(t/deftest construct-component-test
  (t/testing "Construction of component"
    (let [component (construct-component service-lifecyle-definition)
          lifecycle @(get-in component [:service :lifecycle])
          reference @(get-in component [:service :reference])
          dependencies @(get-in component [:service :dependencies])
          configuration @(get-in component [:service :configuration])]
      (t/is
        (and lifecycle reference dependencies configuration)
        "Component should have lifecycle, reference, dependencies and configuration referencies")
      (t/is (=
            {:constructor 'clojure-swarm.composit-test/service-constructor
             :configurator 'clojure-swarm.composit-test/service-configurator
             :cleaner 'clojure-swarm.composit-test/service-cleaner}
            lifecycle)
            "Component should have expected lifecycle")
      (t/is (=
            {:enabled? true}
            reference)
            "Component should be constructed properly"))))

(t/deftest construct-system-test
  (t/testing "Construction of system"
    (let [system (construct-system [service-lifecyle-definition client-lifecyle-definition])]
      (t/is (:service system) "System should have service component")
      (t/is (:client system) "System should have client component"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest inject-component-dependencies-test
  (t/testing "Injection of component dependecies"
    (let [system (construct-system [service-lifecyle-definition client-lifecyle-definition])
          service-component (:service system)
          _ (inject-component-dependencies! system service-component service-dependecies)
          dependencies @(:dependencies service-component)
          service @(:reference service-component)]
      (t/is (= service-dependecies dependencies) "Dependencies should be set.")
      (t/is (:discoverer service) "Service should have discoverer reference"))))

(t/deftest inject-system-dependencies-test
  (t/testing "Injection of system dependencies"
    (let [system (construct-system [service-lifecyle-definition client-lifecyle-definition])
          _ (inject-system-dependencies! system {:service service-dependecies})
          service-component (:service system)
          dependencies @(:dependencies service-component)
          service @(:reference service-component)]
      (t/is (= service-dependecies dependencies) "Dependencies should be set.")
      (t/is (:discoverer service) "Service should have discoverer reference"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest configure-component-test
  (t/testing "Configuration of component"
    (let [component (:service (construct-component service-lifecyle-definition))
          _ (configure-component! component service-configuration)
          curr-configuration @(:configuration component)
          reference @(:reference component)]
      (t/is (:timeout reference) "Service should have timeout key.")
      (t/is (= service-configuration curr-configuration) "Configuration should be set."))))

(t/deftest configure-system-test
  (t/testing "Configuration of component"
    (let [system (construct-system [service-lifecyle-definition client-lifecyle-definition])
          _ (configure-system! system {:service service-configuration})
          component (:service system)
          curr-configuration @(:configuration component)
          reference @(:reference component)]
      (t/is (:timeout reference) "Service should have timeout key.")
      (t/is (= service-configuration curr-configuration) "Configuration should be set."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest cleanup-component-test
  (t/testing "Component cleanup"
    (let [component (:service (construct-component service-lifecyle-definition))
          _ (cleanup-component! component)
          enabled? (:enabled? @(:reference component))]
      (t/is (not enabled?) "Service must be disabled."))))

(t/deftest cleanup-component-test
  (t/testing "Component cleanup"
    (let [system (construct-system [service-lifecyle-definition client-lifecyle-definition])
          _ (cleanup-system! system)
          service-enabled? (:enabled? @(get-in system [:service :reference]))
          client-enabled? (:enabled? @(get-in system [:client :reference]))]
      (t/is (not service-enabled?) "Service must be disabled.")
      (t/is (not client-enabled?) "Client must be disabled."))))
