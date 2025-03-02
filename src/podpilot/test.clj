;;; =========================================================================
;;; podpilot_test.clj - Tests for PodPilot
;;; =========================================================================

(ns podpilot.test
  (:require [podpilot.mock :as mock]
            [podpilot.core :as core]
            [clojure.test :refer [deftest is testing]]))

;;; =========================================================================
;;; Helper Functions
;;; =========================================================================

(defn with-mock-api
  "Runs the provided function with mocked API functions"
  [f]
  (with-redefs [core/graphql-request mock/mock-graphql-request
                core/register-managed-pod mock/mock-register-managed-pod
                core/unregister-managed-pod mock/mock-unregister-managed-pod
                core/get-managed-pods mock/mock-get-managed-pods
                core/test-ssh-connection mock/mock-test-ssh-connection
                core/ssh-execute mock/mock-ssh-execute
                core/scp-to-pod mock/mock-scp-to-pod
                core/scp-from-pod mock/mock-scp-from-pod
                core/load-config mock/mock-load-config
                core/save-config mock/mock-save-config
                core/update-config mock/mock-update-config
                core/load-preset mock/mock-load-preset
                core/save-preset mock/mock-save-preset
                core/list-presets mock/mock-list-presets]
    (mock/reset-mock-state!)
    (f)))

;;; =========================================================================
;;; API Tests
;;; =========================================================================

(deftest test-get-pod
  (with-mock-api
    (fn []
      (testing "Getting a valid pod"
        (let [pod (core/get-pod "pod123")]
          (is (= "pod123" (:id pod)))
          (is (= "RUNNING" (:desiredStatus pod)))))

      (testing "Getting a non-existent pod"
        (let [pod (core/get-pod "non-existent")]
          (is (nil? pod)))))))

(deftest test-list-pods
  (with-mock-api
    (fn []
      (testing "Listing all pods"
        (let [pods (core/list-pods)]
          (is (= 2 (count pods)))
          (is (= "pod123" (:id (first pods))))
          (is (= "pod456" (:id (second pods)))))))))

(deftest test-create-pod
  (with-mock-api
    (fn []
      (testing "Creating a new pod"
        (let [config {:gpu-type "NVIDIA A4000"
                      :gpu-count 1
                      :image "test/image:latest"
                      :container-disk-size 15
                      :volume-size 30}
              pod (core/create-pod config)]
          (is (string? (:id pod)))
          (is (= "RUNNING" (:desiredStatus pod)))
          (is (= "test/image:latest" (:imageName pod)))
          (is (= 1 (:gpuCount pod))))))))

(deftest test-terminate-pod
  (with-mock-api
    (fn []
      (testing "Terminating an existing pod"
        (let [result (core/terminate-pod "pod123")]
          (is (true? result))
          (is (= 1 (count (core/list-pods))))))

      (testing "Terminating a non-existent pod"
        (let [result (core/terminate-pod "non-existent")]
          (is (false? (nil? result))))))))

(deftest test-managed-pods
  (with-mock-api
    (fn []
      (testing "Getting managed pods"
        (let [pods (core/get-managed-pods)]
          (is (= #{"pod123"} pods))))

      (testing "Registering a managed pod"
        (core/register-managed-pod "new-pod")
        (let [pods (core/get-managed-pods)]
          (is (= #{"pod123" "new-pod"} pods))))

      (testing "Unregistering a managed pod"
        (core/unregister-managed-pod "pod123")
        (let [pods (core/get-managed-pods)]
          (is (= #{"new-pod"} pods)))))))

(deftest test-pod-ssh-details
  (with-mock-api
    (fn []
      (testing "Getting SSH details for a valid pod"
        (let [details (core/get-pod-ssh-details "pod123")]
          (is (= "10.0.0.1" (:host details)))
          (is (= 10022 (:port details)))
          (is (= "root" (:username details)))))

      (testing "Getting SSH details for a non-existent pod"
        (let [details (core/get-pod-ssh-details "non-existent")]
          (is (nil? details)))))))

;;; =========================================================================
;;; SSH Tests
;;; =========================================================================

(deftest test-ssh-operations
  (with-mock-api
    (fn []
      (testing "Testing SSH connection to valid pod"
        (is (true? (core/test-ssh-connection "pod123"))))

      (testing "Testing SSH connection to non-running pod"
        (is (false? (core/test-ssh-connection "pod456"))))

      (testing "Executing command on valid pod"
        (let [result (core/ssh-execute "pod123" "echo 'test'")]
          (is (= 0 (:exit result)))
          (is (string? (:out result)))))

      (testing "Executing command on non-running pod"
        (let [result (core/ssh-execute "pod456" "echo 'test'")]
          (is (= 1 (:exit result)))
          (is (string? (:err result)))))

      (testing "Transferring file to valid pod"
        (let [result (core/scp-to-pod "pod123" "local/path" "/remote/path")]
          (is (= 0 (:exit result)))))

      (testing "Transferring file from valid pod"
        (let [result (core/scp-from-pod "pod123" "/remote/path" "local/path")]
          (is (= 0 (:exit result))))))))

;;; =========================================================================
;;; Config Tests
;;; =========================================================================

(deftest test-config-operations
  (with-mock-api
    (fn []
      (testing "Loading configuration"
        (let [config (core/load-config)]
          (is (= "mock-api-key" (:api-key config)))
          (is (map? (:defaults config)))))

      (testing "Updating configuration"
        (let [config (core/update-config :api-key "new-api-key")]
          (is (= "new-api-key" (:api-key config)))))

      (testing "Loading preset"
        (let [preset (core/load-preset "test-preset")]
          (is (= "NVIDIA A100" (:gpu-type preset)))
          (is (= 2 (:gpu-count preset)))))

      (testing "Saving preset"
        (let [new-preset {:gpu-type "NVIDIA A6000" :gpu-count 4}
              presets (core/save-preset "new-preset" new-preset)
              loaded (core/load-preset "new-preset")]
          (is (= "NVIDIA A6000" (:gpu-type loaded)))
          (is (= 4 (:gpu-count loaded))))))))

;;; =========================================================================
;;; Command Tests
;;; =========================================================================

(deftest test-cmd-test
  (with-mock-api
    (fn []
      (testing "Running test command with mock API"
        (let [result (core/cmd-test {:opts {:gpu-type "NVIDIA A4000"
                                            :gpu-count 1}
                                     :args ["test/file.py"]})]
          (is (map? result))
          (is (string? (:pod-id result)))
          (is (number? (:exit-code result)))
          (is (number? (:auto-termination-at result))))))))

(deftest test-cmd-list
  (with-mock-api
    (fn []
      (testing "Running list command with mock API"
        (let [result (core/cmd-list {:opts {} :args []})]
          (is (vector? result))
          (is (= 2 (count result))))))))

(deftest test-cmd-kill
  (with-mock-api
    (fn []
      (testing "Killing a specific pod"
        (let [result (core/cmd-kill {:opts {} :args ["pod123"]})]
          (is (true? result))
          (is (= 1 (count (core/list-pods))))
          (is (not (mock/mock-pod-exists? {:id "pod123"}))))))))

(deftest test-cmd-user
  (with-mock-api
    (fn []
      (testing "Retrieving user information"
        (let [result (core/cmd-user {:opts {} :args []})]
          (is (= "user123" (:id result)))
          (is (= "user@example.com" (:email result)))
          (is (= 120.50 (:clientBalance result)))
          (is (= 2 (count (:teams result))))
          (is (= 1 (count (:ownedTeams result)))))))))

;;; =========================================================================
;;; Run Tests
;;; =========================================================================

(defn run-tests []
  (clojure.test/run-tests 'podpilot.test))

(defn -main [& args]
  (let [result (run-tests)]
    (System/exit (if (= 0 (+ (:fail result) (:error result))) 0 1))))

(when (= *file* (System/getProperty "babashka.file"))
  (run-tests))
