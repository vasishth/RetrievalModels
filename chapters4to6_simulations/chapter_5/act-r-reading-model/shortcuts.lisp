(defun rs (sn &optional (params nil))
	(read-sentence sn params))

(defun ra (times &optional (params nil))
	(read-all-times times params))

(defun rl (limit &optional (params nil))
	(read-list limit params))
