;;; =========================================================================
;;; podpilot_mock.clj - Mock API for testing PodPilot
;;; =========================================================================

(ns podpilot.mock
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;;; =========================================================================
;;; Mock Data
;;; =========================================================================

(def mock-pods (atom [{:id "pod123"
                       :name "Test Pod 1"
                       :imageName "runpod/pytorch:latest"
                       :desiredStatus "RUNNING"
                       :machineId "machine123"
                       :costPerHr 0.5
                       :gpuCount 1
                       :uptimeSeconds 3600
                       :runtime {:ports [{:ip "10.0.0.1"
                                          :privatePort 22
                                          :publicPort 10022
                                          :isIpPublic false
                                          :type "tcp"}
                                         {:ip "10.0.0.1"
                                          :privatePort 8888
                                          :publicPort 18888
                                          :isIpPublic false
                                          :type "http"}]}}
                      {:id "pod456"
                       :name "Test Pod 2"
                       :imageName "tensorflow/tensorflow:latest"
                       :desiredStatus "EXITED"
                       :machineId "machine456"
                       :costPerHr 1.0
                       :gpuCount 2
                       :uptimeSeconds nil
                       :runtime nil}]))

(def mock-gpu-types (atom [{:id "NVIDIA A4000"
                            :displayName "A4000"
                            :memoryInGb 16
                            :secureCloud true
                            :communityCloud true
                            :lowestPrice {:minimumBidPrice 0.3
                                          :uninterruptablePrice 0.6}}
                           {:id "NVIDIA A100"
                            :displayName "A100"
                            :memoryInGb 40
                            :secureCloud true
                            :communityCloud false
                            :lowestPrice {:minimumBidPrice 1.2
                                          :uninterruptablePrice 2.0}}]))

(def mock-managed-pods (atom #{"pod123"}))

;; Mock user information
(def mock-user-info
  {:id "user123"
   :email "user@example.com"
   :clientBalance 120.50
   :spendLimit 500
   :currentSpendPerHr 1.25
   :teams [{:id "team1"
            :name "Development Team"
            :owner {:email "owner@example.com"}
            :membership {:scopes {"role" "admin"}}}
           {:id "team2"
            :name "Research Group"
            :owner {:email "researcher@example.com"}
            :membership {:scopes {"role" "member"}}}]
   :ownedTeams [{:id "team3"
                 :name "My Team"
                 :members [{:id "member1"
                            :member {:email "member1@example.com"}
                            :scopes {"role" "admin"}}
                           {:id "member2"
                            :member {:email "member2@example.com"}
                            :scopes {"role" "member"}}]}]})

;;; =========================================================================
;;; Mock Functions for Testing
;;; =========================================================================

(defn reset-mock-state!
  "Resets all mock state back to defaults for clean test runs"
  []
  (reset! mock-pods [{:id "pod123"
                      :name "Test Pod 1"
                      :imageName "runpod/pytorch:latest"
                      :desiredStatus "RUNNING"
                      :machineId "machine123"
                      :costPerHr 0.5
                      :gpuCount 1
                      :uptimeSeconds 3600
                      :runtime {:ports [{:ip "10.0.0.1"
                                         :privatePort 22
                                         :publicPort 10022
                                         :isIpPublic false
                                         :type "tcp"}
                                        {:ip "10.0.0.1"
                                         :privatePort 8888
                                         :publicPort 18888
                                         :isIpPublic false
                                         :type "http"}]}}
                     {:id "pod456"
                      :name "Test Pod 2"
                      :imageName "tensorflow/tensorflow:latest"
                      :desiredStatus "EXITED"
                      :machineId "machine456"
                      :costPerHr 1.0
                      :gpuCount 2
                      :uptimeSeconds nil
                      :runtime nil}])
  (reset! mock-managed-pods #{"pod123"}))

(defn mock-graphql-request
  "Mock implementation of graphql-request function for testing"
  [query variables]
  (cond
    ;; Query for user information
    (str/includes? query "myself {")
    {:data {:myself mock-user-info}}

    ;; Query for listing pods
    (str/includes? query "myself {
            pods {")
    {:data {:myself {:pods @mock-pods}}}

    ;; Query for getting a specific pod
    (str/includes? query "pod(input: $input)")
    (let [pod-id (get-in variables [:input :podId])
          pod (first (filter #(= (:id %) pod-id) @mock-pods))]
      {:data {:pod pod}})

    ;; Query for GPU types
    (str/includes? query "gpuTypes")
    {:data {:gpuTypes @mock-gpu-types}}

    ;; Mutation for creating a pod
    (str/includes? query "podFindAndDeployOnDemand(input: $input)")
    (let [gpu-type (get-in variables [:input :gpuTypeId])
          gpu-count (get-in variables [:input :gpuCount])
          image-name (get-in variables [:input :imageName])
          new-pod-id (str "pod" (rand-int 1000))
          new-pod {:id new-pod-id
                   :name (str "New Pod " new-pod-id)
                   :imageName image-name
                   :desiredStatus "RUNNING"
                   :machineId (str "machine" (rand-int 1000))
                   :gpuCount gpu-count
                   :costPerHr (if (= gpu-type "NVIDIA A100") 2.0 0.5)
                   :runtime {:ports [{:ip "10.0.0.1"
                                      :privatePort 22
                                      :publicPort 10022
                                      :isIpPublic false
                                      :type "tcp"}
                                     {:ip "10.0.0.1"
                                      :privatePort 8888
                                      :publicPort 18888
                                      :isIpPublic false
                                      :type "http"}]}}]
      (swap! mock-pods conj new-pod)
      {:data {:podFindAndDeployOnDemand new-pod}})

    ;; Mutation for terminating a pod
    (str/includes? query "podTerminate(input: $input)")
    (let [pod-id (get-in variables [:input :podId])
          pod-exists? (some #(= (:id %) pod-id) @mock-pods)]
      (when pod-exists?
        (swap! mock-pods (fn [pods] (filter #(not= (:id %) pod-id) pods))))
      {:data {:podTerminate nil}})

    ;; Mutation for stopping a pod
    (str/includes? query "podStop(input: $input)")
    (let [pod-id (get-in variables [:input :podId])
          pod-idx (first (keep-indexed #(when (= (:id %2) pod-id) %1) @mock-pods))]
      (when pod-idx
        (swap! mock-pods update-in [pod-idx :desiredStatus] (constantly "EXITED")))
      {:data {:podStop (get @mock-pods pod-idx)}})

    ;; Default response for unhandled queries
    :else
    {:errors [{:message (str "Unhandled mock query: " (subs query 0 50) "...")}]}))

(defn mock-register-managed-pod
  "Mock implementation of register-managed-pod for testing"
  [pod-id]
  (swap! mock-managed-pods conj pod-id)
  @mock-managed-pods)

(defn mock-unregister-managed-pod
  "Mock implementation of unregister-managed-pod for testing"
  [pod-id]
  (swap! mock-managed-pods disj pod-id)
  @mock-managed-pods)

(defn mock-get-managed-pods
  "Mock implementation of get-managed-pods for testing"
  []
  @mock-managed-pods)

(defn mock-test-ssh-connection
  "Mock implementation of test-ssh-connection for testing"
  [pod-id]
  ;; Only return true for "running" pods
  (let [pod (first (filter #(= (:id %) pod-id) @mock-pods))]
    (= (:desiredStatus pod) "RUNNING")))

(defn mock-ssh-execute
  "Mock implementation of ssh-execute for testing"
  [pod-id command & opts]
  (let [pod (first (filter #(= (:id %) pod-id) @mock-pods))]
    (if (= (:desiredStatus pod) "RUNNING")
      ;; Return success for running pods
      {:exit 0
       :out (str "Executed command: " command)
       :err ""}
      ;; Return error for non-running pods
      {:exit 1
       :out ""
       :err "Connection refused"})))

(defn mock-scp-to-pod
  "Mock implementation of scp-to-pod for testing"
  [pod-id local-path remote-path]
  (let [pod (first (filter #(= (:id %) pod-id) @mock-pods))]
    (if (= (:desiredStatus pod) "RUNNING")
      ;; Return success for running pods
      {:exit 0
       :out (str "Copied " local-path " to " remote-path)
       :err ""}
      ;; Return error for non-running pods
      {:exit 1
       :out ""
       :err "Connection refused"})))

(defn mock-scp-from-pod
  "Mock implementation of scp-from-pod for testing"
  [pod-id remote-path local-path]
  (let [pod (first (filter #(= (:id %) pod-id) @mock-pods))]
    (if (= (:desiredStatus pod) "RUNNING")
      ;; Return success for running pods
      {:exit 0
       :out (str "Copied " remote-path " to " local-path)
       :err ""}
      ;; Return error for non-running pods
      {:exit 1
       :out ""
       :err "Connection refused"})))

;; Mock config functions
(def mock-config (atom {:api-key "mock-api-key"
                        :defaults {:gpu-type "NVIDIA A4000"
                                   :gpu-count 1
                                   :container-disk-size 10
                                   :volume-size 20
                                   :image "runpod/pytorch:latest"
                                   :timeout 30}}))

(def mock-presets (atom {"test-preset" {:gpu-type "NVIDIA A100"
                                        :gpu-count 2
                                        :image "tensorflow/tensorflow:latest"
                                        :timeout 60}}))

(defn mock-load-config
  "Mock implementation of load-config for testing"
  []
  @mock-config)

(defn mock-save-config
  "Mock implementation of save-config for testing"
  [config]
  (reset! mock-config config)
  config)

(defn mock-update-config
  "Mock implementation of update-config for testing"
  [key value]
  (swap! mock-config assoc key value)
  @mock-config)

(defn mock-load-preset
  "Mock implementation of load-preset for testing"
  [name]
  (get @mock-presets name))

(defn mock-save-preset
  "Mock implementation of save-preset for testing"
  [name config]
  (swap! mock-presets assoc name config)
  @mock-presets)

(defn mock-list-presets
  "Mock implementation of list-presets for testing"
  []
  @mock-presets)

;; Mock helper to verify a pod with specific properties exists
(defn mock-pod-exists?
  "Checks if a pod with specific properties exists in the mock database"
  [props]
  (some #(every? (fn [[k v]] (= (get % k) v)) props) @mock-pods))
