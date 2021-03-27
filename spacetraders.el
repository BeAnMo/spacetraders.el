;; Notes
;; Need tables - loans/ships/cargo
;; Mapping out systems
;; Interactive tables

;;{"token":"c9b966de-5588-4375-86b9-1d4d24b46adb","user":{"id":"ckmaqc05d11880721cs6oklgyfe0","username":"GazNougat","picture":null,"email":null,"credits":0,"createdAt":"2021-03-15T15:16:25.154Z","updatedAt":"2021-03-15T15:16:25.154Z"}}

(require 'request)
(require 'cl) ;; lexical-let is not automatically available?
(setq debug-on-error t)

(defgroup spacetraders nil
  "Spacetraders config options."
  :prefix "spacetraders-"
  :group 'games)

(defcustom spacetraders-username ""
  "User name."
  :type 'string)
(defcustom spacetraders-userid ""
  "User ID."
  :type 'string)
(defcustom spacetraders-token ""
  "User token."
  :type 'string)
(defvar-local spacetraders-user-created-at ""
  "When the current user/token was generated. Data is wiped after 1 week on Sunday.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Request token ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun st-fetch-token (username)
  (interactive
   (let ((username (or spacetraders-username
		       (read-string "Username? "))))
     (list username)))
  (st-api-call
   (format "users/%s/token" username)
   :method "post"
   :callback (cl-function
	      (lambda (&key data &key error-thrown &allow-other-keys)
		(progn
		  (message "ST:[fetch/token]")
		  (pp data)
		  (setq spacetraders-token
			(assoc-default 'token` data))
		  (let ((user (assoc-default 'user data)))
		    (setq spacetraders-user-created-at
			  (assoc-default 'createdAt user))
		    (setq spacetraders-username
			  (assoc-default 'username user))))))))
			  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Current states ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar st-user nil
  "User response.")
(defvar st-system nil
  "System info for current ship.")
(defvar st-marketplace nil
  "Current marketplace state.")
(defvar st-flightplan nil
  "Current flight-plan.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Active selections ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar-local st-current-ship nil
  "Selected ship ID.")
(defvar-local st-current-flight-plan nil
  "Selected flight plan ID.")
(defvar-local st-current-location nil
  "Location of selected ship.")
(defvar-local st-current-loan nil
  "Selected loan ID.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State listeners ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar-local st-state-subscriptions nil
  "Subscriptions to state changes.")

(defun st-state-add-listener (name listener)
  "Subscribe a function to state changes.

NAME: symbol
LISTENER: () => Any"
  (progn
    (st-state-remove-listener name)
     (setq st-state-subscriptions
	(cons (cons name listener)
	      st-state-subscriptions))))

(defun st-state-remove-listener (name)
  "Unsubscribe from state changes.

NAME: symbol"
  (let ((filtered (seq-filter
		   (lambda (pair)
		     (not (eq (car pair) name)))
		   st-state-subscriptions)))
    (setq st-state-subscriptions filtered)))

(defun st-action-apply-middleware (dispatch middleware)
  "Returns the enhanced version of dispatch.

DISPATCH: Action => nil
MIDDLEWARE: (list DISPATCH => Action => nil)"
  (if (null middleware)
      dispatch
    (st-action-apply-middleware
     (funcall (car middleware) dispatch)
     (cdr middleware))))

(defun st-action-dispatch (action)
  (progn
    ;; Write to state.
    (st-action-reducer action)
    ;; Call subscriptions with current action.
    (dolist (pair st-state-subscriptions)
	(funcall (cdr pair) action))))

(defun st-middleware-logger (dispatch)
  (lexical-let ((next dispatch))
    (lambda (action)
      (progn
	(message "ST:[Next action] %s" (car action))
	(funcall next action)))))

(defun st-middleware-thunk (dispatch)
  (lexical-let ((next dispatch))
    (lambda (action)
      (if (functionp action)
	  (funcall action next)
	(funcall next action)))))

(defvar-local st-state-dispatch
  (st-action-apply-middleware 'st-action-dispatch
			      (list 'st-middleware-logger
				    'st-middleware-thunk))
  "Enhanced global action dispatch.")

;; FLOW
;; Fetch user
;; select current ship
;; -> fetch current system
;;   -> fetch current marketplace
;;
;; Posting a flightplan creates a delay.
;; No location data will be available for the current
;; ship until the destination is reached.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App reducer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun st-action-reducer (action)
  "Updates global state based on the given ACTION.

Returns true if changes occurred, nil otherwise.
ACTION: (cons symbol any)"
  (let* ((type (car action))
	 (payload (cdr action))
	 (result 
	  (pcase type
	    ('retrieved/user (setq st-user payload))
	    ('set/current-ship (setq st-current-ship payload))
	    ('retrieved/system (setq st-system payload))
	    ('retrieved/marketplace (setq st-marketplace payload))
	    ('retrieved/flightplan (setq st-flightplan payload))
	    (non '__NO_WRITE__))))
    (not (eq result '__NO_WRITE__))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spacetraders helper functions ;;;;;;;;;;;;;;;;;
(defmacro st-api-callback (proc)
  `(function*
    (lambda (&key data &key error-thrown &allow-other-keys)
      (when (not (null error-thrown))
	(message "API error: %s" error-thrown))
      (funcall ,proc data))))

(defun st-api-url (pathname)
  (format "https://api.spacetraders.io/%s"
	  pathname))

(defun st-api-call (endpoint &rest args)
  (let ((method (get-or args :method "GET"))
	(headers (get-or args :headers nil))
	(params (get-or args :params nil))
	(data (get-or args :data nil))
	(callback (get-or args :callback nil)))
    (request
      (st-api-url endpoint)
      :type method
      :parser 'json-read
      :params params
      :data data
      :headers headers
      :complete callback)))

(defun st-get (selector)
  "Print the current state of a given selector function."
  (interactive
   (let ((selector (read-string "Selector? ")))
     (list selector)))
  (let* ((key (intern (format "st-get-%s" selector)))
	 (value (funcall key)))
    ;; Can't use (get-state key), why?
    (pp value)))

(defun st-query (query)
  (interactive
   (let ((query (read-string "Query? ")))
     (list query)))
  (let ((key (intern (format "st-query-%s" query))))
    (call-interactively key)))

(defun st-fetch (entity)
  (interactive
   (let ((entity (read-string "Entity? ")))
     (list entity)))
  (let ((key (intern (format "st-fetch-%s" entity))))
    (call-interactively key)))

(defun st-post (entity)
  (interactive
   (let ((entity (read-string "Entity? ")))
     (list entity)))
  (let ((key (intern (format "st-post-%s" entity))))
    (call-interactively key)))

(defun st-read-options (options)
  "Takes a list of primitives to map options to.
eg: '(a b c) -> '((1 . a) (2 . b) (3 . c))"
  (let ((selected (read-number
		   (mapconcat
		    (lambda (item)
		      (format "%d: %s" (car item) (cdr item)))
		    options
		    "; ")
		   (mapcar #'car options))))
    (assoc-default selected options)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(st-state-add-listener
 'fetch-user
 (lambda (action)
   (when (eq (car action) 'fetch/user)
     (st-fetch-user))))

(defun st-action-retrieved/user (data)
  (cons 'retrieved/user data))

(defun st-get-user ()
  (assoc-default 'user st-user))

(defun st-get-user-credits ()
  (assoc-default 'credits (st-get-user)))

(defun st-get-user-debts ()
  (seq-reduce
   (lambda (acc item)
     (+ acc (assoc-default 'repaymentAmount item)))
   (st-get-user-loans)
   0))

(defun st-get-user-balance ()
  (- (st-get-user-credits)
     (st-get-user-debts)))

(cl-defun st-get-user-loans (&optional (use-list t))
  (let ((items (assoc-default 'loans (st-get-user))))
    (if use-list (seq-into items 'list) items)))

(defun st-get-user-loan-options ()
  (st-enumerate-options
   (seq-map (apply-partially 'assoc-default 'id)
	    (st-get-user-loans))))

(cl-defun st-get-user-ships (&optional (use-list t))
  (let ((val (assoc-default 'ships (st-get-user))))
    (if use-list (seq-into val 'list) val)))

(defun st-options-user-ships ()
  (st-enumerate-options
   (seq-map (apply-partially 'assoc-default 'id)
	    (st-get-user-ships))))

(cl-defun st-dispatch-fetch-user (&key data &key error-thrown &allow-other-keys)
  (if (not (null error-thrown))
      (progn
	(message "ST:ERR[fetch/user] %s" error-thrown)
	(pp data))
    (funcall st-state-dispatch
	     (st-action-retrieved/user data))))
  
(defun st-fetch-user ()
  "Fetch & set the current user state."
  (interactive)
  (st-api-call
   (format "users/%s" spacetraders-username)
   :headers '((Content-Type . "application/json"))
   :params `((token . ,spacetraders-token))
   :callback 'st-dispatch-fetch-user))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ship functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun st-action-set/current-ship (ship-id)
  (cons 'set/current-ship ship-id))

(defun st-get-ship ()
  (let ((found (seq-filter
		(lambda (s)
		  (string= st-current-ship (assoc-default 'id s)))
		(st-get-user-ships))))
    (first-or-nil found)))

(defun st-get-ship-space-available ()
  (assoc-default 'spaceAvailable (st-get-ship)))

(defun st-get-ship-fuel ()
  (let ((maybe (seq-filter
		(lambda (item)
		  (string= (assoc-default 'good item)
			   "FUEL"))
		(st-get-ship-cargo t))))
    (if (null maybe)
	0
      (assoc-default 'quantity (car maybe)))))

(defun st-get-ship-location ()
  (assoc-default 'location (st-get-ship)))

(defun st-get-ship-system ()
  (let ((val (st-get-ship-location)))
    (if (null val) "" (substring val 0 2))))

(defun st-get-ship-coords ()
  (let ((s (st-get-ship)))
    (cons (assoc-default 'x s)
	  (assoc-default 'y s))))

(cl-defun st-get-ship-cargo (&optional (use-list t))
  (let ((items (assoc-default 'cargo (st-get-ship))))
    (if use-list (seq-into items 'list) items)))

(defun st-options-ship-cargo ()
  (st-enumerate-options
   (seq-map (apply-partially 'assoc-default 'good)
	    (st-get-ship-cargo))))

(defun st-action-set/current-ship (ship-id)
  (cons 'set/current-ship ship-id))

(defun st-set-current-ship (ship-id)
  "Select the current ship-id for operations."
  (interactive
   (let ((ship-id (st-read-options (st-options-user-ships))))
     (list ship-id)))
  (progn
    (funcall st-state-dispatch
	     (st-action-set/current-ship ship-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ship Purchases ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar-local st-ships-for-sale nil
  "Local state to handle ship purchases.")

(cl-defun st-get-ships-for-sale (&optional (use-list t))
  (let ((items (assoc-default 'ships st-ships-for-sale)))
    (if use-list (seq-into items 'list) items)))

(defun st-get-normalized-ships-for-sale ()
  (unwind-and-flatten (st-get-ships-for-sale)
		      'purchaseLocations))

(defun st-get-ships-for-sale-options ()
  (st-enumerate-options
   (seq-map (lambda (d)
	      (list (assoc-default 'manufacturer d)
		    (assoc-default 'class d)
		    (assoc-default 'location (assoc-default 'purchaseLocations d))
		    (assoc-default 'price (assoc-default 'purchaseLocations d))))
	    (st-get-normalized-ships-for-sale))))

(defun st-options-ship-purchase-locations ()
  (st-enumerate-options
   (seq-map (lambda (d)
	      (assoc-default 'location (assoc-default 'purchaseLocations d)))
	    (st-get-normalized-ships-for-sale))))

(defun st-select-normalized-ship-location (ship)
  (assoc-default 'location
		 (assoc-default 'purchaseLocations ship)))

(defun st-options-ship-purchase-types (&optional loc-sym)
  (let* ((ships (st-get-normalized-ships-for-sale))
	 (prepped (if (null loc-sym)
		      ships
		    (seq-filter
		     (lambda (d)
		       (string= loc-sym
			        (st-select-normalized-ship-location d)))
		     ships))))
    (st-enumerate-options
     (seq-map (apply-partially 'type) prepped))))

(defun st-display-ships-for-sale ()
  (interactive)
  (write-table-to-buffer
   "*ST: Ships for sale*"
   (ass-table
    (unwind-and-flatten (st-get-ships-for-sale) 'purchaseLocations)
    (apply-partially 'assoc 'weapons)
    (apply-partially 'assoc 'plating)
    (apply-partially 'assoc 'speed)
    (apply-partially 'assoc 'maxCargo)
    (lambda (row)
      (assoc 'location (assoc-default 'purchaseLocations row)))
    (lambda (row)
      (assoc 'price (assoc-default 'purchaseLocations row)))
    (apply-partially 'assoc 'class)
    (apply-partially 'assoc 'type)
    (apply-partially 'assoc 'manufacturer))))

(defun st-fetch-ships-for-sale ()
  (interactive)
  (st-api-call "game/ships"
	       :params `((token . ,spacetraders-token))
	       :callback (cl-function
			  (lambda (&key data &key error-thrown &allow-other-keys)
			    (setq st-ships-for-sale data)
			    (message "[Fetch ships for sale]")))))

(defun st-post-ship-purchase (loc-sym ship-type)
  (interactive
   (let* ((loc-sym (st-read-options (st-options-ship-purchase-locations)))
	  (ship-type (st-read-options (st-options-ship-purchase-types loc-sym))))
     (list loc-sym ship-type)))
  (let ((endpoint (format "users/%s/ships" spacetraders-username)))
    (st-api-call
     endpoint
     :method "post"
     :params `((token . ,spacetraders-token))
     :headers `((Content-Type . "application/json"))
     :data (json-encode `((location . ,loc-sym)
			  (type . ,ship-type)))
     :callback (cl-function
		(lambda (&key data &key error-thrown &allow-other-keys)
		  (if error-thrown
		      (message "ST-ERR:[%s] &s" endpoint error-thrown)
		    (progn
		      (message "[Purchased ship]")
		      (pp data)
		      (funcall st-state-dispatch
			       (cons 'fetch/user nil))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System & Location functions ;;;;;;;;;;;;;;;;;;;
(st-state-add-listener
 'fetch-system-on-ship-change
 (lambda (action)
   (when (eq (car action) 'set/current-ship)
     (st-fetch-system (st-get-ship-system)))))

(defun st-action-retrieved/system (data)
  (cons 'retrieved/system data))

(cl-defun st-get-system (&optional (use-list t))
  (let ((items (assoc-default 'locations st-system)))
    (if use-list (seq-into items 'list) items)))

(defun st-get-system-name ()
  (st-get-ship-location))

(defun st-get-location ()
  (pairs-string= 'symbol
		 (st-get-ship-location)
		 (st-get-system)))

(defun st-options-locations ()
  (st-enumerate-options
   (seq-map (apply-partially 'assoc-default 'symbol)
	    (st-get-system))))

(defun st-location-coords (loc-obj)
  (cons (assoc-default 'x loc-obj)
	(assoc-default 'y loc-obj)))

(defun st-arg-get-location-coords (loc-sym)
  "Takes a location symbol (string) and returns its coordinates as a pair.

LOC-SYM: string"
  (let ((found (pairs-string= 'symbol loc-sym (st-get-system))))
    (if (null found)
        nil
      (st-location-coords found))))

(defun st-query-distance-from-location (loc)
  "Calculates the distances between the chosen location and the current location.

LOC: string (location's symbol)"
  (interactive
   (let ((loc (st-read-options (st-options-locations))))
     (list loc)))
  (let ((other-coords (st-arg-get-location-coords loc)))
    (if (null other-coords)
	-1
      (message
       "Distance from %s to %s is %s."
       (st-get-ship-location)
       loc
       (format
	"%0.2f"
	(st-distance-between (st-arg-get-location-coords (st-get-ship-location))
			     other-coords))))))

(defun st-distance-between (p1 p2)
  "Gets the distance between 2 coordinates.

P1: (cons number number)
P2: (cons number numer)"
  (let ((x-diff (- (car p1) (car p2)))
	(y-diff (- (cdr p1) (cdr p2))))
    (sqrt (+ (* x-diff x-diff)
	     (* y-diff y-diff)))))

(defun st-display-system ()
  "Create a table of current system data."
  (interactive)
  (write-table-to-buffer
   (format "*ST: %s System" (st-get-ship-system))
   (ass-table (st-get-system)
	      (apply-partially 'assoc 'symbol)
	      (apply-partially 'assoc 'type)
	      (apply-partially 'assoc 'name)
	      (apply-partially 'assoc 'x)
	      (apply-partially 'assoc 'y)
	      (lambda (row)
		(cons
		 'distance
		 (st-distance-between  (st-location-coords (st-get-location))
				       (st-location-coords row)))))))

(cl-defun st-dispatch-system (&key data &key error-thrown &allow-other-keys)
  (if error-thrown
      (progn
	(message "ST:ERR[fetch/system] %s" error-thrown)
	(pp data))
    (funcall st-state-dispatch
	     (st-action-retrieved/system data))))

(defun st-fetch-system (loc)
  (interactive
   (let ((loc (or (st-get-ship-system)
		  (read-string "Symbol? "))))
     (list loc)))
  (st-api-call (format "game/systems/%s/locations" loc)
	    :params `((token . ,spacetraders-token))
	    :callback 'st-dispatch-system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flightplans ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun st-action-retrieved/flightplan (data)
  (lambda (dispatch)
    (progn
      (funcall dispatch (cons 'retrieved/flightplan data))
      (setq eta-timer
	    (run-with-timer
	     (+ (assoc-default
		 'timeRemainingInSeconds
		 (assoc-default 'flightPlan data))
		5)
	     nil
	     (lambda (_dispatch)
	       (progn
		 (message "ST:[Retrieving updated flight data.]")
		 ;; Refresh user.
		 (funcall _dispatch (cons 'fetch/user nil))
		 ;; Refresh system & marketplace.
		 (when st-current-ship
		   (funcall _dispatch (cons 'set/current-ship st-current-ship)))
		 (message "ST:[Cancelling 'eta-timer.]")
		 (cancel-timer eta-timer)))
	     dispatch)))))

(defun st-get-flightplan ()
  (assoc-default 'flightPlan st-flightplan))

(defun st-get-flightplan-id ()
  (assoc-default 'id (st-get-flightplan)))

(defun st-get-flightplan-time-remaining ()
  (assoc-default 'timeRemainingInSeconds (st-get-flightplan)))

(cl-defun st-dispatch-flightplan (&key data &key error-thrown &allow-other-keys)
  (if error-thrown
      (progn
	(message "ST:ERR[post/flightplan] %s" error-thrown)
	(pp data))
    (progn
      (pp data)
      (funcall st-state-dispatch
	       (st-action-retrieved/flightplan data))
      (message "ST:[post/flightplan] Current ship ETA is %s seconds."
	       (st-get-flightplan-time-remaining)))))

(defun st-post-flightplan (loc)
  "Create a flight plan within the current system."
  (interactive
   (let ((loc (st-read-options (st-options-locations))))
     (list loc)))
  (st-api-call
   (format "users/%s/flight-plans" spacetraders-username)
   :method "post"
   :headers `((Content-Type . "application/json"))
   :params `((token . ,spacetraders-token))
   :data (json-encode `((shipId . ,st-current-ship)
			(destination . ,loc)))
   :callback 'st-dispatch-flightplan))

(defun st-fetch-flightplan (flightplan-id)
  "Get flightplan data."
  (interactive
   (let ((flightplan-id (read-string "Flightplan ID? ")))
     (list flightplan-id)))
  (st-api-call
   (format "users/%s/flight-plans/%s"
	   spacetraders-username
	   flightplan-id)
   :headers `((Content-Type . "application/json"))
   :params `((token . ,spacetraders-token))
   :callback (cl-function
	      (lambda (&key data &key error-thrown &allow-other-keys)
		(if error-thrown
		    (message "ST:ERR[fetch/flightplan] %s"
			     (assoc-default 'message data))
		  (progn
		    (message "ST:[fetch/flightplan] Retrieved flightplan.")
		    (pp d)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Marketplace functions ;;;;;;;;;;;;;;;;;;;;;;;;;
(st-state-add-listener
 'fetch-marketplace-on-system-change
 (lambda (action)
   (when (eq (car action) 'retrieved/system)
     (st-fetch-marketplace (st-get-ship-location)))))

(defun st-action-retrieved/marketplace (data)
  (cons 'retrieved/marketplace data))

(defun st-get-marketplace ()
  (assoc-default 'location st-marketplace))

(defun st-get-marketplace-name ()
  (assoc-default 'name (st-get-marketplace)))

(cl-defun st-get-marketplace-goods (&optional (use-list t))
  (let ((items (assoc-default 'marketplace (st-get-marketplace))))
    (if use-list (seq-into items 'list) items)))

(defun st-options-marketplace-goods ()
  (st-enumerate-options
   (seq-map (apply-partially 'assoc-default 'symbol)
	    (st-get-marketplace-goods))))

(defun st-options-ship-cargo ()
  (st-enumerate-options
   (seq-map (apply-partially 'assoc-default 'good)
	    (st-get-ship-cargo))))

(defun st-display-marketplace ()
  (interactive)
  (write-table-to-buffer
   (format "*ST: %s Marketplace*" (st-get-marketplace-name))
   (ass-table (st-get-marketplace-goods)
	      (apply-partially 'assoc 'volumePerUnit)
	      (apply-partially 'assoc 'quantityAvailable)
	      (apply-partially 'assoc 'pricePerUnit)
	      (apply-partially 'assoc 'symbol))))

(cl-defun st-dispatch-marketplace (&key data &key error-thrown &allow-other-keys)
  (if error-thrown
      (progn
	(message "ST:ERR[fetch/marketplace] %s" error-thrown)
	(pp data))
    (funcall st-state-dispatch
	     (st-action-retrieved/marketplace data))))

(defun st-fetch-marketplace (loc)
  (interactive
   (let ((loc (or (st-get-ship-location)
		  (read-string "Location Symbol? "))))
     (list loc)))
  (st-api-call
   (format "game/locations/%s/marketplace" loc)
   :params `((token . ,spacetraders-token))
   :callback 'st-dispatch-marketplace))

(cl-defun st-dispatch-sell (&key data &key error-thrown &allow-other-keys)
  (if error-thrown
      (progn
	(message "ST:ERR[post/sell] %s" error-thrown)
	(pp data))
    (funcall st-state-dispatch
	     (cons 'fetch/user nil))))

(defun st-post-sell (good quantity)
   (interactive
    (let* ((good (st-read-options (st-options-ship-cargo)))
	   (quantity (read-number (format "Quantity of %s? " good))))
      (list good quantity)))
   (st-api-call (format "users/%s/sell-orders" spacetraders-username)
	       :method "post"
	       :headers `((Content-Type . "application/json"))
	       :params `((token . ,spacetraders-token))
	       :data (json-encode `((shipId . ,st-current-ship)
				    (good . ,good)
				    (quantity . ,quantity)))
	       :callback 'st-dispatch-sell))

(defun st-post-buy (good quantity)
  (interactive
   (let* ((good (st-read-options (st-options-marketplace-goods)))
	  (quantity (read-number (format "Quantity of %s? " good))))
     (list good quantity)))
  (st-api-call (format "users/%s/purchase-orders" spacetraders-username)
	       :method "post"
	       :headers `((Content-Type . "application/json"))
	       :params `((token . ,spacetraders-token))
	       :data (json-encode `((shipId . ,st-current-ship)
				    (good . ,good)
				    (quantity . ,quantity)))
	       :callback (st-api-callback
			  (lambda (d)
			    (progn
			      (message "Made purchase.")
			      (pp d)
			      (funcall st-state-dispatch
				       (cons 'fetch/user nil)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loan functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Not part of global user state.
(defvar-local st-loans nil
  "Current loans available")

(cl-defun st-get-loans (&optional (use-list t))
  (let ((items (assoc-default 'loans st-loans)))
    (if use-list (seq-into items 'list) items)))

(defun st-options-loans ()
  (st-enumerate-options
   (seq-map (apply-partially 'type) (st-get-loans))))

(defun st-fetch-loans ()
  (interactive)
  (st-api-call "game/loans"
	       :params `((token . ,spacetraders-token))
	       :callback (st-api-callback
			  (lambda (d)
			    (setq st-loans d)
			    (message "[Fetched loan data]")))))

(defun st-post-loan (loan-type)
  (interactive
   (let ((loan-type (st-read-options (st-options-loans))))
     (list loan-type)))
  (st-api-call (format "users/%s/loans" spacetraders-username)
	       :method "post"
	       :headers `((Content-Type ."application/json"))
	       :params `((token . , spacetraders-token))
	       :data (json-encode `((type . ,loan-type)))
	       :callback (st-api-callback
			  (lambda (d)
			    (message "[Fetched loan]")
			    (pp d)
			    (funcall st-state-dispatch
				     (cons 'fetch/user nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun st-fetch-game-status ()
  (interactive)
  (st-api-call "game/status"
	       :callback (st-api-callback
			  (lambda (d)
			    (let ((found (assoc-default 'status d)))
			      (if (null found)
				  (message "%s" d)
				(message "%s" found)))))))


(defun st-get-all-systems ()
  (interactive)
  (st-api-call "game/systems"
	       :params `((token . ,st-token))
	       :callback (st-api-callback
			  (lambda (d)
			    (progn
			      (putq-state systems d)
			      (message "Retrieved systems data."))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table Functions KEEP ;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unwind-json (row unwind-key)
  (let ((nested (assoc-default unwind-key row))
	(copied (assq-delete-all unwind-key (copy-alist row))))
    (seq-map
     (lambda (nested-row)
       (append `((,unwind-key . ,nested-row)) copied))
     nested)))

(defun unwind-and-flatten (rows unwind-key)
  "Unwind a list of assoc at the given UNWIND-KEY.

ROW: (list assoc-list)
UNWIND-KEY: symbol

ex: '((a . 1) (b . (1 2 3)) (c . 3)) -> 
'((a . 1) (b . 1) (b . 2) (b . 3) (c . 3))"
  (defun iter (a-list acc)
    (if (null a-list)
	acc
      (iter (cdr a-list)
	    (append acc (unwind-json (car a-list) unwind-key)))))

  (iter rows '()))

(defun select-keys (ass-list keys)
  (mapcar (lambda (pairs)
	    (seq-filter (lambda (pair)
			  (member (car pair) keys))
			pairs))
	  ass-list))

(defun select-headers (ass-list)
  (mapcar (lambda (pair) (car pair))
	  (car ass-list)))

(defun select-rows (ass-list)
  (mapcar (lambda (pairs)
	    (mapcar (lambda (pair) (cdr pair))
		    pairs))
	  ass-list))


(defun pluck-from-ass (selectors pairs)
  "Returns mapped selectors with reversed list.

SELECTORS: List-of [Assoc -> Assoc]
PAIRS: (list ...assoc-list))"
  (defun iter (acc procs)
    (if (null procs)
	acc
      (let ((proc (car procs))
	    (rest (cdr procs)))
	(iter (cons (funcall proc pairs) acc)
	      rest))))
  
  (iter '() selectors))

(defun ass-table (pairs &rest selectors)
  "Builds a list of lists structured like a CSV file with headers.

PAIRS: List-of-Assoc-List
SELECTORS: Assoc-List -> Assoc-List"
  (let* ((projected (seq-map
		     (apply-partially 'pluck-from-ass selectors)
		     pairs)))
    (cons (select-headers projected)
	  (select-rows projected))))

(defun expr-replace (expr placeholder value)
  "Replaces a symbol in an arbitrarily nested sexpr with the given value.

EXPR: List
PLACEHOLDER: symbol
VALUE: any"
  (defun atomp (expr)
    (or (numberp expr)
	(symbolp expr)
	(stringp expr)))
  
  (defun pairp (exp)
    (and (listp expr) (atomp (cdr expr))))
  
  (defun swap-atom (exp)
    (if (eq exp placeholder)
	value
      exp))
  
  (defun traverse (exp)
    (cond
     ((null exp) nil)
     ((pairp exp) (cons (decide (car exp)) (decide (cdr exp))))
     ((cons (decide (car exp)) (traverse (cdr exp))))))
  
  (defun decide (exp)
    (if (atomp exp)
	(swap-atom exp)
      (traverse exp)))
  
  (decide expr))

(defun replace-holders (originals replacements)
  (cond
   ((null originals) nil)
   ((and (symbolp (car originals))
	     (string= (substring (symbol-name (car originals)) 0 1)
		      "$")) 
    (cons (car replacements)
	  (replace-holders (cdr originals)
			   (cdr replacements))))
   (:else (cons (car originals)
		(replace-holders (cdr originals)
				 replacements)))))

  
(defmacro $> (form)
  "Binds a lambda to FORM, allowing use of the placeholder symbol '$'.

FORM: List

ex: ($> (+ 1 $0 3 $1)) expands to (lambda (x y) (+ 1 x 3 y))

"
  (lexical-let ((fn (car form))
		(args0 (cdr form)))
    (cond
     ((null args0) (lambda (&rest args1) (apply fn args1)))
     (:else (lambda (&rest args1)
	      (apply fn (replace-holders args0 args1)))))))

(defun write-table-to-buffer (name table &optional use-org-mode)
  "Creates a text table in a new buffer called NAME.

NAME: string
TABLE: (cons headers rows) 
  headers: (list any)
  rows: (list any)
USE-ORG-MODE?: boolean"
  (defun insert-col (col)
    (insert (format "%s&" col)))
  (defun insert-row (row)
    (progn
      (mapcar 'insert-col row)
      (insert "\n")))
  
  (let ((new-buffer (get-buffer-create name)))
    (with-current-buffer new-buffer
      (erase-buffer)
      (when use-org-mode (org-mode))
      (mapcar 'insert-row table)
      (table-capture 1 (point-max) "&" "\n" nil nil nil)
      (buffer-substring-no-properties (point-min)
				      (point-max)))))


(defun st-write-into-buffer (name &rest components)
  (let ((new-buffer (get-buffer-create name)))
    (with-current-buffer new-buffer
      (erase-buffer)
      (dolist (component components)
	;; Need to keep track of posn in buffer
	;; in order to insert components properly.
	(funcall component)
	(insert "\n")))))


(defun val-to-string (val)
  (if (stringp val)
      val
    (format "%s" val)))

(defun st-display-table (title table)
  (defun insert-row (row)
    (progn
      (insert (string-join (seq-map 'val-to-string row) ","))
      (insert "\n")))
    (progn
      (org-mode)
      (insert (format "%s\n" title))
      (let ((prev-max (point-max)))
	(mapcar 'insert-row table))))

(defun st-get-user-ship-status ()
  (ass-table (st-get-user-ships)
	     (apply-partially 'assoc 'id)
	     (apply-partially 'assoc 'location)
	     (apply-partially 'assoc 'spaceAvailable)
	     (apply-partially 'assoc 'type)
	     (apply-partially 'assoc 'class)
	     (apply-partially 'assoc 'maxCargo)
	     (apply-partially 'assoc 'speed)
	     (apply-partially 'assoc 'manufacturer)
	     (apply-partially 'assoc 'plating)
	     (apply-partially 'assoc 'weapons)))

(defun st-get-user-loan-status ()
   (ass-table (st-get-user-loans)
	      (apply-partially 'assoc 'type)
	      (apply-partially 'assoc 'status)
	      (lambda (d) (cons 'amount (assoc-default 'repaymentAmount d)))
	      (apply-partially 'assoc 'due)
	      (apply-partially 'assoc 'id)))

(defun st-status-ship-cargo ()
  (ass-table (st-get-ship-cargo)
	     (apply-partially 'assoc 'good)
	     (apply-partially 'assoc 'quantity)
	     (apply-partially 'assoc 'totalVolume)))

(defun st-get-user-status ()
  (list (cons 'credits (st-get-user-credits))
	(cons 'debts (st-get-user-debts))
	(cons 'balance (st-get-user-balance))
	(cons 'number-of-ships (length (st-get-user-ships)))
	(cons 'number-of-loans (length (st-get-user-loans)))))
  
(defun st-display-u-list (title a-list)
  "Inserts an unordered-list into the current buffer.

TITLE: string
A-LIST: List-of-Assoc"
  (defun insert-pair (pair)
    (insert (format "- %s %s\n"
		    (car pair)
		    (cdr pair))))
  (progn
    (org-mode)
    (insert (format "%s\n" title))
    (seq-map 'insert-pair a-list)))

(defun st-view-user ()
  (interactive)
  ;; Must make sure data is in memory before calling new buffer.
  (let ((local-user-state
	 (apply-partially 'st-display-u-list
			  (format "* Current status for %s" spacetraders-username)
			  (st-get-user-status)))
	(local-ship-state
	 (apply-partially 'st-display-table
			  "* Ships"
			  (st-get-user-ship-status)))
	(local-loan-state
	 (apply-partially 'st-display-table
			  "* Loans"
			  (st-get-user-loan-status)))
	(local-cargo
	 (apply-partially 'st-display-table
			  "* Current Ship Cargo"
			  (st-status-ship-cargo))))
    (st-write-into-buffer
     (format "*ST: %s View*" spacetraders-username)
     local-user-state
     local-loan-state
     local-ship-state
     local-cargo)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-or (args key default)
  "Returns the value of KEY in ARGS otherwise DEFAULT"
  (or (plist-get args key)
      default))

(defun first-or-nil (a-list)
  (if (null a-list)
      nil
    (car a-list)))

(defun pairs-string= (key val a-list)
  "Returns the first element whose key equals val."
  (let ((found (seq-filter
		(lambda (s)
		  (string= val (assoc-default key s)))
		a-list)))
    (first-or-nil found)))

(defun st-enumerate-options (a-list)
  "Takes a list and assign a numeric selector for interactive option menus

A-LIST: List-of-Any"
  (loop for i
	from 1
	for item in a-list
	collect (cons i item)))

;; https://github.com/kiwanami/emacs-ctable
;;(require 'ctable)

;; general notes
;; Nyon METALS(3) -> Prime|Obo METALS(8)
;; Tritus CHEMICALS(7) -> Ucarro|Ado CHEMICALS(11)
;; Ucarro WORKERS(23) -> Tritus|Bo WORKERS(30)
;; Obo CONSTRUCTION_MATERIALS(230) -> Bo CONSTRUCTION_MATERIALS(300)

;; Nyon metal -> Obo const-mat -> Bo research? -> Prime machinery -> Nyon

;; fuel efficiency ~3.35
(defun st-automate ()
  (list
   ;; (cons action payload)
   (cons 'post/buy (list "FUEL" 'refill-min-amount))
   (cons 'post/buy (list "METALS" 'fill-remaining))
   (cons 'post/flightplan "OE-NY")
   (cons 'post/sell (list "METALS" 'sell-all))
   (cons 'post/buy (list "FUEL" 'refill-min-amount))
   (cons 'post/buy (list "CONSTRUCTION_MATERIAL" 'fill-remaining))
   (cons 'post/flightplan "OE-UC-OB")
   (cons 'post/sell (list "CONSTRUCTION_MATERIAL" 'sell-all))
   (cons 'post/buy '("FUEL" refill-min-amount))
   (cons 'post/buy "")))
   
