(defmodule beas
 (export all))

;;;--------------------------------------------------------------------
;;; Keys
;;;--------------------------------------------------------------------
(defsyntax key-email-to-uid-ptr
 ([in-email] (: eru er_key 'beas 'email in-email)))

; store all usernames as lowercase
(defun key-username-to-uid-ptr
 ([username] (when (is_list username))
  (: eru er_key 'beas 'username (: string to_lower username)))
 ([username] (when (is_binary username))
  (key-username-to-uid-ptr
   (: unicode characters_to_list (binary_to_list username))))
 ([username] (when (is_atom username))
  (key-username-to-uid-ptr
   (atom_to_binary username 'utf8))))

(defsyntax key-priv-level-ptr
 ([in-priv-level] (: eru er_key 'beas 'priv-level in-priv-level)))

(defsyntax key-user-hash
 ([uid] (: eru er_key 'beas 'user uid)))

(defsyntax key-user-feature-hash
 ([uid] (: eru er_key 'beas 'user uid 'features)))

(defsyntax key-location-marker
 ([marker-id] (: eru er_key 'beas 'marker marker-id)))

(defsyntax key-payment-provider-uid-map
 ([payment-provider provider-uid]
  (: eru er_key 'beas 'provider-map payment-provider provider-uid)))

(defsyntax key-payment-record
 ([uid] (: eru er_key 'beas 'payments uid)))

(defsyntax key-user-location-marker
 ([uid] (: eru er_key 'beas 'marks uid)))

;;;--------------------------------------------------------------------
;;; Counter Keys
;;;--------------------------------------------------------------------
(defsyntax key-counter-user
 ([] (: eru er_key 'beas 'counter 'user)))

(defsyntax key-counter-location-marker
 ([] (: eru er_key 'beas 'counter 'location-marker)))

;;;--------------------------------------------------------------------
;;; helpful conversions
;;;--------------------------------------------------------------------
(defsyntax to-int
 ([redis-val] (list_to_integer (binary_to_list redis-val))))

;;;--------------------------------------------------------------------
;;; Atomic Helpers
;;;--------------------------------------------------------------------
(defmacro atomic-set (key success failure)
 `(case (: er setnx redis ,key 'test)
   ('true ,@success)
   ('false ,@failure)))

;;;--------------------------------------------------------------------
;;; User Creation
;;;--------------------------------------------------------------------
(defun user-create (redis username email password)
 (atomic-set (key-username-to-uid-ptr username)
  ; success -- the username didn't exist
  ((atomic-set (key-email-to-uid-ptr email)
   ; success -- the email didn't exist
   ((let* ((uid (: er incr redis (key-counter-user)))
          (user-key (key-user-hash uid)))
    (: er set redis (key-username-to-uid-ptr username) uid)
    (: er hset redis user-key 'username username)
    (user-email-raw redis uid 'nil email)
    (: er hset redis user-key 'ts-signup (now-s))
    (password-set redis user-key password)
    uid))
   ; failure -- the username didn't exist, but the email did
   ((: er del redis (key-username-to-uid-ptr username)) ; free up the username
                                                  ; we aren't making the account
   'email_exists)))
  ; failure -- username already exists
  ('user_exists)))

(defun password-set (redis key password)
 (let ((cycles 400000))
  (: er hset redis key 'password (password-encode password cycles))
  (: er hset redis key 'passenc cycles)))

(defun password-encode
 ([password 0] password)
 ([password n] (when (> n 0))
  (password-encode (: crypto sha password) (- n 1))))

;;;--------------------------------------------------------------------
;;; User Updating
;;;--------------------------------------------------------------------
(defun user-email (redis uid email)
 (atomic-set (key-email-to-uid-ptr email) ; attempt to set new email addr
  ; set success
  ((user-email-raw redis uid (user-email redis uid) email))
  ; set failure
  ('email_exists)))

(defun user-email-raw (redis uid existing-email new-email)
 (case existing-email
  ('nil 'nil) ; this is an initial set, so nothing to pre-delete
  (e (: er del redis (key-email-to-uid-ptr e)))) ; remove existing email to UID
 (: er set redis (key-email-to-uid-ptr new-email) uid) ; add new email->UID
 (: er hset redis (key-user-hash uid) 'email new-email)) ; update user hash

(defun user-disable (redis uid)
 (: er hset redis (key-user-hash uid) 'disabled (now-s)))

(defun user-enable (redis uid)
 (: er hdel redis (key-user-hash uid) 'disabled))

(defun user-feature-set (redis uid feature value)
 (: er hset redis (key-user-feature-hash uid) feature value))

(defun user-feature-del (redis uid feature)
 (: er hdel redis (key-user-feature-hash uid) feature))

(defun user-feature-incr (redis uid feature delta)
 (: er hincrby redis (key-user-feature-hash uid) feature delta))

;;;--------------------------------------------------------------------
;;; User Reading
;;;--------------------------------------------------------------------
(defun username-exists (redis username)
 (: er exists redis (key-username-to-uid-ptr username)))

(defun email-exists (redis email)
 (: er exists redis (key-email-to-uid-ptr email)))

(defun username-to-uid (redis username)
 (: er get redis (key-username-to-uid-ptr username)))

(defun email-to-uid (redis email)
 (: er get redis (key-email-to-uid-ptr email)))

(defun user-username (redis uid)
 (: er hget redis (key-user-hash uid) 'username))

(defun user-email (redis uid)
 (: er hget redis (key-user-hash uid) 'email))

(defun user-password-match (redis uid attempted-password)
 (let* ((bin-password-cycles (: er hget redis (key-user-hash uid) 'passenc))
        (password-cycles (list_to_integer (binary_to_list bin-password-cycles)))
        (existing-password (: er hget redis (key-user-hash uid) 'password))
        (computed-attempt (password-encode attempted-password password-cycles)))
 (=:= computed-attempt existing-password)))

(defun user-signup-ts (redis uid)
 (to-int (: er hget redis (key-user-hash uid) 'ts-signup)))

(defun user-is-disabled (redis uid)
 (: er hexists redis (key-user-hash uid) 'disabled))

(defun user-feature-value (redis uid feature)
 (: er hget redis (key-user-feature-hash uid) feature))

; yes, this is redundant, but -value is dumb
(defun user-feature-get (redis uid feature)
 (user-feature-value redis uid feature))

; yes, this is redundant, but -value is dumb
(defun user-feature (redis uid feature)
 (user-feature-value redis uid feature))

(defun user-feature-exists (redis uid feature)
 (: er hexists redis (key-user-feature-hash uid) feature))

(defun user-features (redis uid)
 (: er hkeys redis (key-user-feature-hash uid)))

;;;--------------------------------------------------------------------
;;; Subscription Updating
;;;--------------------------------------------------------------------
(defun user-subscribe (redis uid plan expires)
 (user-feature-set redis uid 'subscription plan)
 (user-subscribe-expire redis uid expires))

(defun user-subscribe-expire (redis uid expires)
 (user-feature-set redis uid 'subscription-expires expires))

(defun user-unsubscribe (redis uid)
 (user-feature-del redis uid 'subscription))

;;;--------------------------------------------------------------------
;;; Subscription Reading
;;;--------------------------------------------------------------------
(defun user-subscription (redis uid)
 (user-feature-value redis uid 'subscription))

(defun user-subscription-expires (redis uid)
 (to-int (user-feature-value redis uid 'subscription-expires)))

;;;--------------------------------------------------------------------
;;; Location Updating
;;;--------------------------------------------------------------------
(defun user-location-marker-new (redis uid name lat lng start stop recur)
 (let ((new-marker-id (key-user-location-marker (gen-marker-id redis))))
  (: er hmset redis new-marker-id
   (list 'uid uid 'name name 'lat lat 'lng lng
         'start start 'stop stop 'recur recur))
  (: er lpush redis (key-user-location-marker uid) new-marker-id)))

(defun gen-marker-id (redis)
 (key-location-marker (: er incr redis (key-counter-location-marker))))

(defun user-location-marker-disable (redis uid marker-id)
 (: er hset redis (key-user-location-marker uid) 'disabled (now-s)))

(defun user-location-marker-is-disabled (redis uid marker-id)
 (: er hget redis (key-user-location-marker uid) 'disabled))

(defun user-location-marker-enable (redis uid marker-id)
 (: er hdel redis (key-user-location-marker uid) 'disabled))

;;;--------------------------------------------------------------------
;;; Location Reading
;;;--------------------------------------------------------------------
(defun user-locations (redis uid)
 (let ((marker-keys (: er lrange redis (key-user-location-marker uid) 0 -1)))
  (lc ((<- key marker-keys)) (: er hgetall_p redis key))))
       
;;;--------------------------------------------------------------------
;;; Payment Updating
;;;--------------------------------------------------------------------
(defun payment-uid-map-set (redis provider provider-id uid)
 ; this er_key isn't a 'beas key, it's just a concat of provider:provider-id
 (user-feature-set redis uid 'payment-uid (: eru er_key provider provider-id))
 (: er set (key-payment-provider-uid-map provider provider-id) uid))

(defun payment-record (redis uid payment-provider invoice-id)
 ; this er_key isn't a 'beas key, it's just a concat of provider:invoice-id
 (: er lpush redis (key-payment-record uid)
  (: eru er_key payment-provider invoice-id)))

;;;--------------------------------------------------------------------
;;; Payment Reading
;;;--------------------------------------------------------------------
(defun payments (redis uid)
 (: er lrange redis (key-payment-record uid) 0 -1))

;;;--------------------------------------------------------------------
;;; Misc
;;;--------------------------------------------------------------------
(defun now-ms ()
 (calc-now-to-ms (now)))

(defun calc-now-to-ms 
 ([(tuple mega sec micro)]
  ; converted from trunc((((Mega * 1000000) + Sec) + (Micro / 1000000)) * 1000)
  (trunc (* (+ (+ (* mega 1000000) sec) (/ micro 1000000)) 1000))))

(defun now-s ()
 (calc-now-to-s (now)))

(defun calc-now-to-s
 ([(tuple mega sec _)]
  (+ (* mega 1000000) sec)))
