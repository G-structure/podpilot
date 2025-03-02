#!/usr/bin/env bb

;;; =========================================================================
;;; PodPilot - RunPod Management CLI Tool
;;; =========================================================================
;;; A tool for managing RunPod instances, executing Python code remotely,
;;; and automatically handling resources efficiently.
;;;
;;; Author: Open AI Assistant
;;; Version: 0.1.0
;;; License: MIT
;;; =========================================================================

(ns podpilot.core
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as process]
            [babashka.http-client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [cheshire.core :as json]))

(declare register-managed-pod unregister-managed-pod get-managed-pods)

;;; =========================================================================
;;; Configuration Management
;;; =========================================================================

;; Returns the path to the podpilot configuration directory
(defn config-dir
  "Returns the absolute path to the PodPilot configuration directory.
   Creates the directory if it doesn't exist.

   Returns:
     String: Path to the configuration directory"
  []
  (let [dir (str (System/getProperty "user.home") "/.podpilot")]
    (when-not (fs/exists? dir)
      (fs/create-dir dir))
    dir))

;; Returns the path to the podpilot configuration file
(defn config-file
  "Returns the absolute path to the main configuration file.

   Returns:
     String: Path to the configuration file"
  []
  (str (config-dir) "/config.edn"))

;; Returns the path to the managed pods tracking file
(defn managed-pods-file
  "Returns the absolute path to the file tracking pods managed by PodPilot.

   Returns:
     String: Path to the managed pods file"
  []
  (str (config-dir) "/managed-pods.edn"))

;; Returns the path to the preset configurations file
(defn presets-file
  "Returns the absolute path to the presets configuration file.

   Returns:
     String: Path to the presets file"
  []
  (str (config-dir) "/presets.edn"))

;; Loads the configuration from the config file
(defn load-config
  "Loads and returns the user configuration.
   Creates default configuration if none exists.

   Returns:
     Map: The configuration map with API key and default settings"
  []
  (if (fs/exists? (config-file))
    (edn/read-string (slurp (config-file)))
    {:api-key nil
     :defaults {:gpu-type "NVIDIA A4000"
                :gpu-count 1
                :container-disk-size 10
                :volume-size 20
                :image "runpod/pytorch:latest"
                :timeout 30
                :team-id nil}}))

;; Saves the configuration to the config file
(defn save-config
  "Saves the provided configuration to the config file.

   Args:
     config (Map): The configuration map to save

   Returns:
     Map: The saved configuration"
  [config]
  (spit (config-file) (pr-str config))
  config)

;; Updates a specific field in the configuration
(defn update-config
  "Updates a specific field in the configuration and saves it.

   Args:
     key (Keyword): The key to update
     value (Any): The new value

   Returns:
     Map: The updated configuration"
  [key value]
  (let [config (load-config)
        updated-config (assoc config key value)]
    (save-config updated-config)))

;; Loads a specific preset from the presets file
(defn load-preset
  "Loads a named preset configuration.

   Args:
     name (String): The name of the preset to load

   Returns:
     Map: The preset configuration or nil if not found"
  [name]
  (when (fs/exists? (presets-file))
    (let [presets (edn/read-string (slurp (presets-file)))]
      (get presets name))))

;; Saves a new preset to the presets file
(defn save-preset
  "Saves a named preset configuration.

   Args:
     name (String): The name for the preset
     config (Map): The configuration to save

   Returns:
     Map: The updated presets map"
  [name config]
  (let [presets-path (presets-file)
        existing-presets (if (fs/exists? presets-path)
                           (edn/read-string (slurp presets-path))
                           {})
        updated-presets (assoc existing-presets name config)]
    (spit presets-path (pr-str updated-presets))
    updated-presets))

;; List all available presets
(defn list-presets
  "Lists all available preset configurations.

   Returns:
     Map: All saved presets"
  []
  (if (fs/exists? (presets-file))
    (edn/read-string (slurp (presets-file)))
    {}))

;;; =========================================================================
;;; RunPod API Integration
;;; =========================================================================

;; Gets the API key from environment or config
(defn get-api-key
  "Retrieves the RunPod API key from environment or configuration.
   Exits with an error if no key is found.

   Returns:
     String: The API key"
  []
  (or (System/getenv "RUNPOD_API_KEY")
      (:api-key (load-config))
      (do
        (println "Error: No RunPod API key found")
        (println "Set it with: podpilot config set-api-key YOUR_KEY")
        (println "Or set the RUNPOD_API_KEY environment variable")
        (System/exit 1))))

;; Makes a GraphQL request to the RunPod API
(defn graphql-request
  "Makes a GraphQL request to the RunPod API.

   Args:
     query (String): The GraphQL query
     variables (Map): Variables for the GraphQL query

   Returns:
     Map: The API response parsed from JSON"
  [query variables]
  (let [response (http/post "https://api.runpod.io/graphql"
                            {:headers {"Content-Type" "application/json"
                                       "Authorization" (str "Bearer " (get-api-key))}
                             :body (json/generate-string
                                    {:query query
                                     :variables (or variables {})})})]
    (json/parse-string (:body response) true)))

;; Creates a new pod with specified configuration
(defn create-pod
  "Creates a new RunPod instance with the specified configuration.

   Args:
     config (Map): Configuration map with gpu-type, gpu-count, etc.

   Returns:
     Map: Information about the created pod including ID"
  [{:keys [gpu-type gpu-count image container-disk-size volume-size team-id]
    :or {gpu-type "NVIDIA A4000"
         gpu-count 1
         image "runpod/pytorch:latest"
         container-disk-size 10
         volume-size 20}}]

  (println "Creating pod with:"
           "GPU:" gpu-type
           "Count:" gpu-count
           "Image:" image
           (when team-id (str "Team ID: " team-id)))

  (let [create-pod-mutation "
        mutation CreatePod($input: PodFindAndDeployOnDemandInput!) {
          podFindAndDeployOnDemand(input: $input) {
            id
            name
            imageName
            machineId
            desiredStatus
            ports
          }
        }
        "
        variables {:input (cond-> {:gpuTypeId gpu-type
                                   :gpuCount (if (string? gpu-count)
                                               (Integer/parseInt gpu-count)
                                               gpu-count)
                                   :containerDiskInGb container-disk-size
                                   :volumeInGb volume-size
                                   :imageName image
                                   :dockerArgs ""
                                   :ports "22/tcp,8888/http"
                                   :volumeMountPath "/workspace"}
                            team-id (assoc :teamId team-id))}
        response (graphql-request create-pod-mutation variables)
        pod (get-in response [:data :podFindAndDeployOnDemand])]

    (if pod
      (do
        (println "Pod created successfully. ID:" (:id pod))
        (register-managed-pod (:id pod))
        pod)
      (do
        (println "Error creating pod: " response)
        (System/exit 1)))))

;; Gets information about a specific pod
(defn get-pod
  "Retrieves information about a specific pod.

   Args:
     pod-id (String): The ID of the pod

   Returns:
     Map: Information about the pod"
  [pod-id]
  (let [query "
        query Pod($input: PodFilter!) {
          pod(input: $input) {
            id
            name
            imageName
            desiredStatus
            machineId
            runtime {
              ports {
                ip
                privatePort
                publicPort
                isIpPublic
                type
              }
            }
          }
        }
        "
        variables {:input {:podId pod-id}}
        response (graphql-request query variables)]
    (get-in response [:data :pod])))

;; Lists all pods for the current user
(defn list-pods
  "Lists all pods for the current user.

   Returns:
     Vector: List of pod information maps"
  []
  (let [query "
        query Pods {
          myself {
            pods {
              id
              name
              desiredStatus
              imageName
              costPerHr
              gpuCount
              uptimeSeconds
            }
          }
        }
        "
        response (graphql-request query {})
        pods (get-in response [:data :myself :pods])]
    pods))

;; Terminates a specific pod
(defn terminate-pod
  "Terminates a specific pod.

   Args:
     pod-id (String): The ID of the pod to terminate

   Returns:
     Boolean: True if termination was successful"
  [pod-id]
  (println "Terminating pod:" pod-id)
  (let [mutation "
        mutation TerminatePod($input: PodTerminateInput!) {
          podTerminate(input: $input)
        }
        "
        variables {:input {:podId pod-id}}
        response (graphql-request mutation variables)]
    (if (nil? (get-in response [:data :podTerminate]))
      (do
        (println "Pod terminated successfully")
        (unregister-managed-pod pod-id)
        true)
      (do
        (println "Error terminating pod:" response)
        false))))

;; Stops a pod (without terminating)
(defn stop-pod
  "Stops a pod without terminating it.

   Args:
     pod-id (String): The ID of the pod to stop

   Returns:
     Map: Information about the stopped pod"
  [pod-id]
  (println "Stopping pod:" pod-id)
  (let [mutation "
        mutation StopPod($input: PodStopInput!) {
          podStop(input: $input) {
            id
            desiredStatus
          }
        }
        "
        variables {:input {:podId pod-id}}
        response (graphql-request mutation variables)]
    (get-in response [:data :podStop])))

;; Waits for a pod to be in the RUNNING state
(defn wait-for-pod-ready
  "Waits for a pod to reach RUNNING state.

   Args:
     pod-id (String): The ID of the pod
     timeout-sec (Integer): Maximum time to wait in seconds (default: 300)

   Returns:
     Boolean: True if pod reached RUNNING state before timeout"
  [pod-id & {:keys [timeout-sec] :or {timeout-sec 300}}]
  (println "Waiting for pod to become ready (timeout:" timeout-sec "seconds)...")
  (loop [elapsed 0
         dots 0]
    (if (>= elapsed timeout-sec)
      (do
        (println "\nTimeout waiting for pod to become ready")
        false)
      (let [pod (get-pod pod-id)
            status (:desiredStatus pod)]
        (if (= "RUNNING" status)
          (do
            (println "\nPod is ready!")
            true)
          (do
            (print ".")
            (flush)
            (Thread/sleep 5000)
            (recur (+ elapsed 5) (inc dots))))))))

;; Gets SSH connection details for a pod
(defn get-pod-ssh-details
  "Extracts SSH connection details from pod information.

   Args:
     pod-id (String): The ID of the pod

   Returns:
     Map: SSH connection details including host, port, etc."
  [pod-id]
  (let [pod (get-pod pod-id)
        ports (get-in pod [:runtime :ports])
        ssh-port (first (filter #(= 22 (:privatePort %)) ports))]
    (when ssh-port
      {:host (:ip ssh-port)
       :port (:publicPort ssh-port)
       :username "root"
       :public-ip (:isIpPublic ssh-port)})))

;; Retrieves user information including team membership details
(defn get-user-info
  "Retrieves information about the current user, including team memberships.

   Args:
   None

   Returns:
   Map: User information including teams, email, credits, etc."
  []
  (let [query "
    query Myself {
      myself {
        id
        email
        clientBalance
        spendLimit
        currentSpendPerHr
        teams {
          id
          name
          owner {
            email
          }
          membership {
            scopes
          }
        }
        ownedTeams {
          id
          name
          members {
            id
            member {
              email
            }
            scopes
          }
        }
      }
    }
  "
        response (graphql-request query {})]
    (get-in response [:data :myself])))

;;; =========================================================================
;;; Pod Management & Tracking
;;; =========================================================================

;; Registers a pod as managed by PodPilot
(defn register-managed-pod
  "Registers a pod as being managed by PodPilot.

   Args:
     pod-id (String): The ID of the pod to register

   Returns:
     Set: Updated set of managed pod IDs"
  [pod-id]
  (let [file (managed-pods-file)
        existing (if (fs/exists? file)
                   (edn/read-string (slurp file))
                   #{})
        updated (conj existing pod-id)]
    (spit file (pr-str updated))
    updated))

;; Unregisters a pod from being managed by PodPilot
(defn unregister-managed-pod
  "Unregisters a pod from being managed by PodPilot.

   Args:
     pod-id (String): The ID of the pod to unregister

   Returns:
     Set: Updated set of managed pod IDs"
  [pod-id]
  (let [file (managed-pods-file)
        existing (if (fs/exists? file)
                   (edn/read-string (slurp file))
                   #{})
        updated (disj existing pod-id)]
    (spit file (pr-str updated))
    updated))

;; Gets all pods managed by PodPilot
(defn get-managed-pods
  "Returns the set of pod IDs managed by PodPilot.

   Returns:
     Set: Set of managed pod IDs"
  []
  (let [file (managed-pods-file)]
    (if (fs/exists? file)
      (edn/read-string (slurp file))
      #{})))

;; Terminates all pods managed by PodPilot (cleanup)
(defn terminate-managed-pods
  "Terminates all pods managed by PodPilot.
   Useful for cleanup on exit.

   Returns:
     Integer: Number of pods terminated"
  []
  (let [pods (get-managed-pods)
        count (count pods)]
    (when (pos? count)
      (println "Cleaning up" count "managed pods..."))
    (doseq [pod-id pods]
      (try
        (terminate-pod pod-id)
        (catch Exception e
          (println "Error terminating pod" pod-id ":" (.getMessage e)))))
    count))

;; Sets up automatic termination for a pod after a timeout period
(defn auto-terminate-after
  "Schedules automatic termination of a pod after specified minutes.
   Runs in a separate thread.

   Args:
     pod-id (String): The ID of the pod to terminate
     minutes (Integer): Minutes to wait before termination

   Returns:
     Future: The future representing the termination task"
  [pod-id minutes]
  (future
    (Thread/sleep (* minutes 60 1000))
    (when (some? (get-pod pod-id))
      (println "\nTimeout reached. Automatically terminating pod:" pod-id)
      (terminate-pod pod-id))))

;; Starts a stopped pod
(defn start-pod
  "Starts a previously stopped pod.

   Args:
     pod-id (String): The ID of the pod to start

   Returns:
     Map: Information about the started pod"
  [pod-id]
  (println "Starting pod:" pod-id)
  (let [mutation "
        mutation ResumePod($input: PodResumeInput!) {
          podResume(input: $input) {
            id
            desiredStatus
            gpuCount
          }
        }
        "
        variables {:input {:podId pod-id}}
        response (graphql-request mutation variables)]
    (let [pod (get-in response [:data :podResume])]
      (if pod
        (do
          (println "Pod started successfully. New status:" (:desiredStatus pod))
          pod)
        (do
          (println "Error starting pod:" response)
          nil)))))

;;; =========================================================================
;;; SSH and Remote Execution
;;; =========================================================================

;; Tests SSH connectivity to a pod
(defn test-ssh-connection
  "Tests SSH connectivity to a pod.

   Args:
     pod-id (String): The ID of the pod to test

   Returns:
     Boolean: True if SSH connection successful"
  [pod-id]
  (let [ssh-details (get-pod-ssh-details pod-id)
        host (:host ssh-details)
        port (:port ssh-details)]
    (try
      (let [result (process/shell
                    {:continue true :out :string :err :string}
                    "ssh" "-o" "StrictHostKeyChecking=no"
                    "-o" "ConnectTimeout=5"
                    "-p" (str port)
                    (str "root@" host)
                    "echo 'SSH connection successful'")]
        (= 0 (:exit result)))
      (catch Exception _
        false))))

;; Waits for SSH to be available on a pod
(defn wait-for-ssh
  "Waits for SSH to become available on a pod.

   Args:
     pod-id (String): The ID of the pod
     timeout-sec (Integer): Maximum time to wait in seconds (default: 120)

   Returns:
     Boolean: True if SSH became available before timeout"
  [pod-id & {:keys [timeout-sec] :or {timeout-sec 120}}]
  (println "Waiting for SSH to become available...")
  (loop [elapsed 0]
    (if (>= elapsed timeout-sec)
      (do
        (println "\nTimeout waiting for SSH")
        false)
      (if (test-ssh-connection pod-id)
        (do
          (println "SSH connection established!")
          true)
        (do
          (print ".")
          (flush)
          (Thread/sleep 5000)
          (recur (+ elapsed 5)))))))

;; Executes a command on a pod via SSH
(defn ssh-execute
  "Executes a command on a pod via SSH.

   Args:
     pod-id (String): The ID of the pod
     command (String): The command to execute
     options (Map): Options like :out and :err handling

   Returns:
     Map: Process result with :exit, :out, and :err keys"
  [pod-id command & {:keys [out err inherit]
                     :or {out :string err :string inherit false}}]
  (let [ssh-details (get-pod-ssh-details pod-id)
        host (:host ssh-details)
        port (:port ssh-details)
        process-opts (cond-> {:continue true :out out :err err}
                       inherit (assoc :inherit inherit))]
    (process/shell
     process-opts
     "ssh" "-o" "StrictHostKeyChecking=no"
     "-p" (str port)
     (str "root@" host)
     command)))

;; Transfers files to a pod using SCP
(defn scp-to-pod
  "Transfers files to a pod using SCP.

   Args:
     pod-id (String): The ID of the pod
     local-path (String): Local file or directory path
     remote-path (String): Remote destination path

   Returns:
     Map: Process result with :exit, :out, and :err keys"
  [pod-id local-path remote-path]
  (let [ssh-details (get-pod-ssh-details pod-id)
        host (:host ssh-details)
        port (:port ssh-details)
        is-dir (fs/directory? local-path)
        args (concat ["scp" "-P" (str port)
                      "-o" "StrictHostKeyChecking=no"]
                     (when is-dir ["-r"])
                     [local-path (str "root@" host ":" remote-path)])]
    (println "Transferring" local-path "to pod...")
    (process/shell {:continue true :inherit true} (str/join " " args))))

;; Transfers files from a pod using SCP
(defn scp-from-pod
  "Transfers files from a pod using SCP.

   Args:
     pod-id (String): The ID of the pod
     remote-path (String): Remote file or directory path
     local-path (String): Local destination path

   Returns:
     Map: Process result with :exit, :out, and :err keys"
  [pod-id remote-path local-path]
  (let [ssh-details (get-pod-ssh-details pod-id)
        host (:host ssh-details)
        port (:port ssh-details)
        args ["scp" "-P" (str port)
              "-o" "StrictHostKeyChecking=no"
              "-r" (str "root@" host ":" remote-path) local-path]]
    (println "Transferring from pod to" local-path "...")
    (process/shell {:continue true :inherit true} (str/join " " args))))

;;; =========================================================================
;;; Command Implementations
;;; =========================================================================

;; Implements the test command
(defn cmd-test
  "Implements the 'test' command to run Python tests on a pod.

   Args:
     opts (Map): Command options
     args (Vector): Command arguments (path to test file/dir)

   Returns:
     Map: Results of the test execution"
  [{:keys [opts args]}]
  (if (empty? args)
    (println "Error: No test file or directory specified")
    (let [path (first args)
          preset (when (:preset opts) (load-preset (:preset opts)))
          config (merge (:defaults (load-config)) preset opts)
          gpu-type (:gpu-type config)
          gpu-count (:gpu-count config)
          image (:image config)
          timeout (or (:timeout config) 30)
          container-disk-size (:container-disk-size config)
          volume-size (:volume-size config)
          team-id (:team-id config)]

      (try
        ;; Create the pod
        (let [pod (create-pod {:gpu-type gpu-type
                               :gpu-count gpu-count
                               :image image
                               :container-disk-size container-disk-size
                               :volume-size volume-size
                               :team-id team-id})
              pod-id (:id pod)]

          ;; Set up auto-termination
          (auto-terminate-after pod-id timeout)

          ;; Wait for pod to be ready
          (when-not (wait-for-pod-ready pod-id)
            (throw (ex-info "Pod never reached ready state" {:pod-id pod-id})))

          ;; Wait for SSH to be available
          (when-not (wait-for-ssh pod-id)
            (throw (ex-info "SSH never became available" {:pod-id pod-id})))

          ;; Transfer the test file or directory
          (let [is-dir (fs/directory? path)
                remote-path (if is-dir "/workspace/tests" "/workspace/test.py")
                result (scp-to-pod pod-id path remote-path)]

            (when-not (zero? (:exit result))
              (throw (ex-info "Failed to transfer test files"
                              {:pod-id pod-id :exit (:exit result)})))

            ;; Run the tests
            (println "\nRunning tests...")
            (let [test-cmd (if is-dir
                             "cd /workspace/tests && python -m pytest -v"
                             "cd /workspace && python test.py")
                  result (ssh-execute pod-id test-cmd :inherit true)]

              (println "\nTest execution completed with exit code:" (:exit result))

              ;; Return information about the test run
              {:pod-id pod-id
               :exit-code (:exit result)
               :auto-termination-at (+ (System/currentTimeMillis)
                                       (* timeout 60 1000))})))

        (catch Exception e
          (println "Error:" (.getMessage e))
          (when-let [data (ex-data e)]
            (when-let [pod-id (:pod-id data)]
              (println "Terminating pod due to error")
              (terminate-pod pod-id)))
          (System/exit 1))))))

;; Implements the exec command
(defn cmd-exec
  "Implements the 'exec' command to execute a command on a pod.

   Args:
     opts (Map): Command options
     args (Vector): Command arguments (command to execute)

   Returns:
     Map: Results of the command execution"
  [{:keys [opts args]}]
  (if (empty? args)
    (println "Error: No command specified")
    (let [command (first args)
          preset (when (:preset opts) (load-preset (:preset opts)))
          config (merge (:defaults (load-config)) preset opts)
          gpu-type (:gpu-type config)
          gpu-count (:gpu-count config)
          image (:image config)
          timeout (or (:timeout config) 30)
          container-disk-size (:container-disk-size config)
          volume-size (:volume-size config)
          team-id (:team-id config)]

      (try
        ;; Create the pod
        (let [pod (create-pod {:gpu-type gpu-type
                               :gpu-count gpu-count
                               :image image
                               :container-disk-size container-disk-size
                               :volume-size volume-size
                               :team-id team-id})
              pod-id (:id pod)]

          ;; Set up auto-termination
          (auto-terminate-after pod-id timeout)

          ;; Wait for pod to be ready
          (when-not (wait-for-pod-ready pod-id)
            (throw (ex-info "Pod never reached ready state" {:pod-id pod-id})))

          ;; Wait for SSH to be available
          (when-not (wait-for-ssh pod-id)
            (throw (ex-info "SSH never became available" {:pod-id pod-id})))

          ;; Execute the command
          (println "\nExecuting command...")
          (let [result (ssh-execute pod-id command :inherit true)]

            (println "\nCommand execution completed with exit code:" (:exit result))

            ;; Return information about the execution
            {:pod-id pod-id
             :exit-code (:exit result)
             :auto-termination-at (+ (System/currentTimeMillis)
                                     (* timeout 60 1000))}))

        (catch Exception e
          (println "Error:" (.getMessage e))
          (when-let [data (ex-data e)]
            (when-let [pod-id (:pod-id data)]
              (println "Terminating pod due to error")
              (terminate-pod pod-id)))
          (System/exit 1))))))

;; Implements the shell command
(defn cmd-shell
  "Implements the 'shell' command to start an interactive shell on a pod.

   Args:
     opts (Map): Command options
     args (Vector): Command arguments (unused)

   Returns:
     Map: Results of the shell session"
  [{:keys [opts _args]}]
  (let [preset (when (:preset opts) (load-preset (:preset opts)))
        config (merge (:defaults (load-config)) preset opts)
        gpu-type (:gpu-type config)
        gpu-count (:gpu-count config)
        image (:image config)
        container-disk-size (:container-disk-size config)
        volume-size (:volume-size config)
        team-id (:team-id config)]

    (try
      ;; Create the pod
      (let [pod (create-pod {:gpu-type gpu-type
                             :gpu-count gpu-count
                             :image image
                             :container-disk-size container-disk-size
                             :volume-size volume-size
                             :team-id team-id})
            pod-id (:id pod)]

        ;; Wait for pod to be ready
        (when-not (wait-for-pod-ready pod-id)
          (throw (ex-info "Pod never reached ready state" {:pod-id pod-id})))

        ;; Wait for SSH to be available
        (when-not (wait-for-ssh pod-id)
          (throw (ex-info "SSH never became available" {:pod-id pod-id})))

        ;; Start interactive shell
        (let [ssh-details (get-pod-ssh-details pod-id)
              host (:host ssh-details)
              port (:port ssh-details)
              args ["ssh" "-o" "StrictHostKeyChecking=no"
                    "-p" (str port)
                    (str "root@" host)]]

          (println "\nStarting interactive shell. Type 'exit' to close session.")
          (println "Warning: Pod will continue running after you exit the shell.")
          (println "Remember to terminate the pod when finished to avoid charges.")
          (println (str "Pod ID: " pod-id "\n"))

          (let [process (process/shell {:inherit true} (str/join " " args))]

            (println "\nShell session ended.")
            (println "Pod is still running. To terminate, use:")
            (println (str "  podpilot kill " pod-id))

            ;; Return information about the shell session
            {:pod-id pod-id
             :exit-code (:exit process)})))

      (catch Exception e
        (println "Error:" (.getMessage e))
        (when-let [data (ex-data e)]
          (when-let [pod-id (:pod-id data)]
            (println "Terminating pod due to error")
            (terminate-pod pod-id)))
        (System/exit 1)))))

;; Implements the list command
(defn cmd-list
  "Implements the 'list' command to list all pods.

   Args:
     opts (Map): Command options (unused)
     args (Vector): Command arguments (unused)

   Returns:
     Vector: List of pods"
  [{:keys [_opts _args]}]
  (let [pods (list-pods)
        managed-pods (get-managed-pods)]

    (if (empty? pods)
      (println "No pods found.")
      (do
        (println (format "%-22s %-15s %-8s %-10s %-8s %s"
                         "ID" "STATUS" "GPU(s)" "COST/HR" "UPTIME" "IMAGE"))

        (println (str/join (repeat 80 "-")))

        (doseq [pod pods]
          (let [pod-id (:id pod)
                managed? (contains? managed-pods pod-id)
                status (:desiredStatus pod)
                gpu-count (:gpuCount pod)
                cost-per-hr (:costPerHr pod)
                uptime-sec (:uptimeSeconds pod)
                uptime-str (if uptime-sec
                             (format "%dh %02dm"
                                     (quot uptime-sec 3600)
                                     (quot (mod uptime-sec 3600) 60))
                             "N/A")
                image (:imageName pod)]

            (println (format "%-22s %-15s %-8d $%-9.2f %-8s %s%s"
                             pod-id
                             status
                             gpu-count
                             (or cost-per-hr 0.0)
                             uptime-str
                             (if (> (count image) 30)
                               (str (subs image 0 27) "...")
                               image)
                             (if managed? " (managed)" "")))))))

    pods))

;; Implements the catalog command
(defn cmd-catalog
  "Implements the 'catalog' command to list available GPU types.

   Args:
     opts (Map): Command options (unused)
     args (Vector): Command arguments (unused)

   Returns:
     Vector: List of GPU types"
  [{:keys [_opts _args]}]
  (let [query "
        query GpuTypes {
          gpuTypes {
            id
            displayName
            memoryInGb
            secureCloud
            communityCloud
            lowestPrice(input: {gpuCount: 1}) {
              minimumBidPrice
              uninterruptablePrice
            }
          }
        }
        "
        response (graphql-request query {})
        gpu-types (get-in response [:data :gpuTypes])]

    (if (empty? gpu-types)
      (println "Failed to retrieve GPU types.")
      (do
        (println (format "%-30s %-12s %-8s %-14s %-14s %s"
                         "GPU TYPE" "VRAM (GB)" "SECURE" "ON-DEMAND" "SPOT" "ID"))

        (println (str/join (repeat 90 "-")))

        (doseq [gpu (sort-by #(get-in % [:lowestPrice :uninterruptablePrice] 0) gpu-types)]
          (let [gpu-id (:id gpu)
                display-name (:displayName gpu)
                memory (:memoryInGb gpu)
                secure? (:secureCloud gpu)
                on-demand-price (get-in gpu [:lowestPrice :uninterruptablePrice])
                spot-price (get-in gpu [:lowestPrice :minimumBidPrice])]

            (println (format "%-30s %-12d %-8s $%-13.2f $%-13.2f %s"
                             display-name
                             memory
                             (if secure? "Yes" "No")
                             (or on-demand-price 0.0)
                             (or spot-price 0.0)
                             gpu-id))))))

    gpu-types))

;; Implements the kill command
(defn cmd-kill
  "Implements the 'kill' command to terminate a pod.

   Args:
     opts (Map): Command options
     args (Vector): Command arguments (pod ID)

   Returns:
     Boolean: True if termination was successful"
  [{:keys [opts args]}]
  (cond
    ;; Kill all idle pods
    (:idle opts)
    (do
      (println "Terminating all idle pods is not yet implemented.")
      false)

    ;; Kill all managed pods
    (:all-managed opts)
    (let [count (terminate-managed-pods)]
      (when (pos? count)
        (println "Terminated" count "managed pods"))
      (pos? count))

    ;; Kill specific pod
    (seq args)
    (terminate-pod (first args))

    ;; No pod ID specified
    :else
    (do
      (println "Error: No pod ID specified")
      (println "Usage: podpilot kill <pod-id>")
      (println "       podpilot kill --idle")
      (println "       podpilot kill --all-managed")
      false)))

;; Implements the config command
(defn cmd-config
  "Implements the 'config' command to manage configuration.

   Args:
     opts (Map): Command options
     args (Vector): Command arguments

   Returns:
     Map: The updated configuration"
  [{:keys [opts args]}]
  (cond
    ;; Set API key
    (:api-key opts)
    (do
      (println "Setting API key...")
      (update-config :api-key (:api-key opts)))

    ;; Save preset
    (:save-preset opts)
    (let [preset-name (first args) ; Get the name from the first argument
          preset-config (select-keys opts [:gpu-type :gpu-count :image
                                           :container-disk-size :volume-size :timeout
                                           :team-id])]
      (if (and preset-name (not (str/starts-with? preset-name "--")))
        (do
          (println "Saving preset:" preset-name)
          (save-preset preset-name preset-config))
        (do
          (println "Error: No valid preset name provided")
          (println "Usage: podpilot config --save-preset <preset-name> [options]")
          nil)))

    ;; List presets
    (:list-presets opts)
    (let [presets (list-presets)]
      (if (empty? presets)
        (println "No presets found.")
        (do
          (println "Available presets:")
          (doseq [[name config] presets]
            (println (format "  %s: %s" name (pr-str config)))))))

    ;; Show current config
    :else
    (let [config (load-config)]
      (println "Current configuration:")
      (println (str "  API Key: " (if (:api-key config) "[Set]" "[Not set]")))
      (println "Default settings:")
      (doseq [[k v] (:defaults config)]
        (println (format "  %s: %s" (name k) v)))
      config)))

;; Implements the transfer command
(defn cmd-transfer
  "Implements the 'transfer' command to transfer files to/from a pod.

   Args:
     opts (Map): Command options
     args (Vector): Command arguments

   Returns:
     Map: Results of the transfer operation"
  [{:keys [opts _args]}]
  (let [pod-id (:pod-id opts)
        local-path (:local opts)
        remote-path (:remote opts)
        direction (:direction opts "to")]

    (cond
      (not pod-id)
      (println "Error: No pod ID specified (use --pod-id)")

      (not local-path)
      (println "Error: No local path specified (use --local)")

      (not remote-path)
      (println "Error: No remote path specified (use --remote)")

      :else
      (case direction
        "to" (scp-to-pod pod-id local-path remote-path)
        "from" (scp-from-pod pod-id remote-path local-path)
        (println "Error: Invalid direction (use --direction to/from)")))))

;; Implements the user command
(defn cmd-user
  "Implements the 'user' command to display user information.

   Args:
   opts (Map): Command options
   args (Vector): Command arguments (unused)

   Returns:
   Map: User information"
  [{:keys [opts _args]}]
  (let [user-info (get-user-info)]
    (if user-info
      (do
        ;; Print basic user information
        (println "\n=== User Information ===")
        (println (format "ID:             %s" (:id user-info)))
        (println (format "Email:          %s" (:email user-info)))

        ;; Fix: Convert clientBalance to Double before formatting
        (println (format "Balance:        $%.2f"
                         (double (or (:clientBalance user-info) 0))))

        ;; Fix: Convert spendLimit to Double and use %.2f instead of %d
        (println (format "Spend Limit:    $%.2f"
                         (double (or (:spendLimit user-info) 0))))

        ;; Fix: Convert currentSpendPerHr to Double before formatting
        (println (format "Current Rate:   $%.2f/hr"
                         (double (or (:currentSpendPerHr user-info) 0))))

        ;; Print teams the user is a member of
        (when (seq (:teams user-info))
          (println "\n=== Team Memberships ===")
          (doseq [team (:teams user-info)]
            (let [scopes (get-in team [:membership :scopes])
                  role (if (map? scopes)
                         (get scopes "role" "member")
                         "member")]
              (println (format "Team: %s (ID: %s)" (:name team) (:id team)))
              (println (format "  Owner: %s" (get-in team [:owner :email])))
              (println (format "  Role: %s" role)))))

        ;; Print teams owned by the user
        (when (seq (:ownedTeams user-info))
          (println "\n=== Teams You Own ===")
          (doseq [team (:ownedTeams user-info)]
            (println (format "Team: %s (ID: %s)" (:name team) (:id team)))
            (when (seq (:members team))
              (println "  Members:")
              (doseq [membership (:members team)]
                (let [scopes (:scopes membership)
                      role (if (map? scopes)
                             (get scopes "role" "member")
                             "member")]
                  (println (format "    %s (Role: %s)"
                                   (get-in membership [:member :email])
                                   role)))))))

        ;; Return the user info for potential further processing
        user-info)
      (do
        (println "Error: Failed to retrieve user information")
        nil))))

;; Implements the start command
(defn cmd-start
  "Implements the 'start' command to start a stopped pod.

   Args:
     opts (Map): Command options
     args (Vector): Command arguments (pod ID)

   Returns:
     Map: Information about the started pod"
  [{:keys [_opts args]}]
  (if (empty? args)
    (println "Error: No pod ID specified")
    (let [pod-id (first args)
          result (start-pod pod-id)]
      (if result
        (do
          (println "Pod is starting. It may take a moment to become ready.")
          (println "Use 'podpilot list' to check its status.")
          result)
        (do
          (println "Failed to start pod" pod-id)
          nil)))))

;; Implements the stop command
(defn cmd-stop
  "Implements the 'stop' command to stop a running pod without terminating it.

   Args:
     opts (Map): Command options
     args (Vector): Command arguments (pod ID)

   Returns:
     Map: Information about the stopped pod"
  [{:keys [_opts args]}]
  (if (empty? args)
    (println "Error: No pod ID specified")
    (let [pod-id (first args)
          result (stop-pod pod-id)]
      (if result
        (do
          (println "Pod stopped successfully.")
          (println "The pod still exists and can be restarted with 'podpilot start'.")
          (println "Note: You'll still be charged for storage while the pod is stopped.")
          result)
        (do
          (println "Failed to stop pod" pod-id)
          nil)))))

;;; =========================================================================
;;; Help and CLI Setup
;;; =========================================================================

;; Prints help information
(defn print-help
  "Prints help information for PodPilot.

   Args:
     command (String, optional): Specific command to show help for

   Returns:
     nil"
  ([]
   (println "PodPilot - RunPod Management Tool")
   (println "Usage: podpilot <command> [options]")
   (println)
   (println "Commands:")
   (println "  test       Run Python tests on a new pod")
   (println "  exec       Execute a command on a new pod")
   (println "  shell      Start an interactive shell on a pod")
   (println "  list       List all active pods")
   (println "  catalog    Show available GPU types with pricing")
   (println "  start      Start a stopped pod")
   (println "  stop       Stop a running pod (without terminating)")
   (println "  kill       Terminate a pod")
   (println "  config     Manage configuration")
   (println "  transfer   Transfer files to/from a pod")
   (println "  user       Display user information and team memberships")
   (println "  help       Show this help or help for a specific command")
   (println)
   (println "Run 'podpilot help <command>' for specific command help"))

  ([command]
   (case command
     "test"
     (do
       (println "podpilot test [options] <test-file-or-directory>")
       (println)
       (println "Options:")
       (println "  --gpu-type TYPE           GPU type to use (default: NVIDIA A4000)")
       (println "  --gpu-count COUNT         Number of GPUs (default: 1)")
       (println "  --image IMAGE             Docker image (default: runpod/pytorch:latest)")
       (println "  --timeout MINUTES         Auto-terminate after minutes (default: 30)")
       (println "  --preset NAME             Use a saved configuration preset")
       (println "  --team-id ID              Team ID to use for pod creation")
       (println "  --container-disk-size GB  Container disk size in GB (default: 10)")
       (println "  --volume-size GB          Volume size in GB (default: 20)"))

     "exec"
     (do
       (println "podpilot exec [options] \"<command>\"")
       (println)
       (println "Options:")
       (println "  --gpu-type TYPE           GPU type to use (default: NVIDIA A4000)")
       (println "  --gpu-count COUNT         Number of GPUs (default: 1)")
       (println "  --image IMAGE             Docker image (default: runpod/pytorch:latest)")
       (println "  --timeout MINUTES         Auto-terminate after minutes (default: 30)")
       (println "  --preset NAME             Use a saved configuration preset")
       (println "  --team-id ID              Team ID to use for pod creation")
       (println "  --container-disk-size GB  Container disk size in GB (default: 10)")
       (println "  --volume-size GB          Volume size in GB (default: 20)"))

     "shell"
     (do
       (println "podpilot shell [options]")
       (println)
       (println "Options:")
       (println "  --gpu-type TYPE           GPU type to use (default: NVIDIA A4000)")
       (println "  --gpu-count COUNT         Number of GPUs (default: 1)")
       (println "  --image IMAGE             Docker image (default: runpod/pytorch:latest)")
       (println "  --preset NAME             Use a saved configuration preset")
       (println "  --team-id ID              Team ID to use for pod creation")
       (println "  --container-disk-size GB  Container disk size in GB (default: 10)")
       (println "  --volume-size GB          Volume size in GB (default: 20)"))

     "list"
     (do
       (println "podpilot list")
       (println)
       (println "Lists all active pods with their status and information."))

     "catalog"
     (do
       (println "podpilot catalog")
       (println)
       (println "Shows available GPU types with pricing information."))

     "start"
     (do
       (println "podpilot start <pod-id>")
       (println)
       (println "Starts a previously stopped pod.")
       (println)
       (println "Arguments:")
       (println "  pod-id      ID of the pod to start"))

     "stop"
     (do
       (println "podpilot stop <pod-id>")
       (println)
       (println "Stops a running pod without terminating it.")
       (println "Note: Storage charges will continue to apply while the pod is stopped.")
       (println)
       (println "Arguments:")
       (println "  pod-id      ID of the pod to stop"))

     "kill"
     (do
       (println "podpilot kill <pod-id>")
       (println "podpilot kill --idle")
       (println "podpilot kill --all-managed")
       (println)
       (println "Options:")
       (println "  --idle           Terminate all idle pods")
       (println "  --all-managed    Terminate all pods managed by PodPilot"))

     "config"
     (do
       (println "podpilot config --api-key YOUR_API_KEY")
       (println "podpilot config --save-preset <preset-name> [--option1 value1 ...]")
       (println "podpilot config --list-presets")
       (println)
       (println "Options:")
       (println "  --api-key KEY             Set your RunPod API key")
       (println "  --save-preset             Save options as a preset with name from args")
       (println "  --list-presets            List all saved presets")
       (println "  --team-id ID              Team ID to use for pod creation")
       (println "  --gpu-type TYPE           GPU type to save in preset")
       (println "  --gpu-count COUNT         GPU count to save in preset")
       (println "  --image IMAGE             Docker image to save in preset")
       (println "  --timeout MINUTES         Timeout to save in preset")
       (println "  --container-disk-size GB  Container disk size to save in preset")
       (println "  --volume-size GB          Volume size to save in preset"))

     "transfer"
     (do
       (println "podpilot transfer --pod-id POD_ID --local LOCAL_PATH --remote REMOTE_PATH [--direction to/from]")
       (println)
       (println "Options:")
       (println "  --pod-id ID        ID of the pod to transfer files to/from")
       (println "  --local PATH       Local file or directory path")
       (println "  --remote PATH      Remote file or directory path on the pod")
       (println "  --direction DIR    Transfer direction: 'to' or 'from' (default: to)"))

     "user"
     (do
       (println "podpilot user")
       (println)
       (println "Displays information about the current user, including:")
       (println "  - Basic account details (email, balance, spend rate)")
       (println "  - Teams the user belongs to and their roles")
       (println "  - Teams owned by the user and their members"))

     ;; Default case - show general help
     (print-help))))

;; Set up shutdown hook for cleanup
(defn setup-shutdown-hook
  "Sets up a JVM shutdown hook to clean up resources.

   Returns:
     nil"
  []
  (-> (Runtime/getRuntime)
      (.addShutdownHook
       (Thread. terminate-managed-pods))))

;;; =========================================================================
;;; Main Entry Point
;;; =========================================================================

(defn -main
  "Main entry point for PodPilot.

   Args:
     & args: Command line arguments

   Returns:
     Any: Result of the command or nil"
  [& args]
  (setup-shutdown-hook)

  (if (empty? args)
    (print-help)
    (let [command (first args)
          remaining-args (vec (rest args))]

      (case command
        "test" (cmd-test {:opts (cli/parse-opts remaining-args) :args remaining-args})
        "exec" (cmd-exec {:opts (cli/parse-opts remaining-args) :args remaining-args})
        "shell" (cmd-shell {:opts (cli/parse-opts remaining-args) :args remaining-args})
        "list" (cmd-list {:opts (cli/parse-opts remaining-args) :args remaining-args})
        "catalog" (cmd-catalog {:opts (cli/parse-opts remaining-args) :args remaining-args})
        "start" (cmd-start {:opts (cli/parse-opts remaining-args) :args remaining-args})
        "stop" (cmd-stop {:opts (cli/parse-opts remaining-args) :args remaining-args})
        "kill" (cmd-kill {:opts (cli/parse-opts remaining-args) :args remaining-args})
        "config"
        (let [opts (cli/parse-opts remaining-args)
              cmd-args (vec (filter #(not (str/starts-with? % "--")) remaining-args))]
          (cmd-config {:opts opts :args cmd-args}))
        "transfer" (cmd-transfer {:opts (cli/parse-opts remaining-args) :args remaining-args})
        "user" (cmd-user {:opts (cli/parse-opts remaining-args) :args remaining-args})
        "help" (if (second args)
                 (print-help (second args))
                 (print-help))

        ;; Default case - unknown command
        (do
          (println "Error: Unknown command:" command)
          (print-help))))))

;; Run main function when script is executed directly
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
