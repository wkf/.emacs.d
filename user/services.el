;;; user.services --- user services configuration
;;; Commentary:
;;; Code:

(require 'prodigy)

(prodigy-define-service
  :name "mesos-master"
  :command "/usr/local/sbin/mesos-master"
  :args '("--ip=127.0.0.1" "--zk=zk://127.0.0.1:2181/mesos" "--quorum=1"
          "--work_dir=/tmp/mesos")
  :tags '(mesos)
  :stop-signal 'kill
  :truncate-output t)
;; "--resources=cpus(*):2.0;cpus(prod):2.0;mem(*):600.0;mem(prod):600.0;ports(*):[30000-31000];ports(prod):[32000-33000]"

(prodigy-define-service
  :name "mesos-slave-1"
  :command "/usr/local/sbin/mesos-slave"
  :args '("--master=127.0.0.1:5050"
          "--no-switch_user"
          "--hostname=localhost"
          "--port=5051"
          "--work_dir=/tmp/mesos_slave_1"
          "--resources=cpus(*):2.0;cpus(prod):2.0;mem(*):600.0;mem(prod):600.0;ports(*):[30000-30999];ports(prod):[31000-31999]"
          "--default_role=prod")
  :tags '(mesos)
  :stop-signal 'kill
  :truncate-output t)

(prodigy-define-service
  :name "mesos-slave-2"
  :command "/usr/local/sbin/mesos-slave"
  :args '("--master=127.0.0.1:5050"
          "--no-switch_user"
          "--hostname=localhost"
          "--port=5052"
          "--work_dir=/tmp/mesos_slave_2"
          "--resources=cpus(*):2.0;cpus(prod):2.0;mem(*):600.0;mem(prod):600.0;ports(*):[32000-32999];ports(prod):[33000-33999]"
          "--default_role=prod")
  :tags '(mesos)
  :stop-signal 'kill
  :truncate-output t)

(prodigy-define-service
  :name "cook"
  :cwd "/Users/will/Dropbox/Projects/Cook"
  :command "lein"
  :args '("run" "dev-config.edn")
  :tags '(cook)
  :stop-signal 'kill)

(provide 'user.services)
;;; services.el ends here
