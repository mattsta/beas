(defmodule beas
 (export all))

;;;--------------------------------------------------------------------
;;; Keys
;;;--------------------------------------------------------------------
(defsyntax key-email-to-uid-ptr
 ([email] (: eru er_key 'email email)))

(defsyntax key-username-to-uid-ptr
 ([username] (: eru er_key 'username username)))

(defsyntax key-priv-level-ptr
 ([priv-level] (: eru er_key 'priv-level priv-level)))

(defsyntax key-user-hash
 ([uid] (: eru er_key 'user uid)))

(defsyntax key-user-feature-hash
 ([uid] (: eru er_key 'user uid 'features)))

(defsyntax key-counter-user
 ([] (: eru er_key 'counter 'user)))

(defsyntax key-counter-location-marker
 ([] (: eru er_key 'counter 'location-marker)))

(defsyntax key-location-marker
 ([marker-id] (: eru er_key 'marker marker-id)))

(defsyntax key-payment-provider-uid-map
 ([payment-provider provider-uid]
  (: eru er_key 'provider-map payment-provider provider-uid)))

(defsyntax key-payment-record
 ([uid] (: eru er_key 'payments uid)))

(defsyntax key-user-location-marker
 ([uid] (: eru er_key 'marks uid)))

;;;--------------------------------------------------------------------
;;; helpful conversions
;;;--------------------------------------------------------------------
(defsyntax to-int
 ([redis-val] (list_to_integer (binary_to_list redis-val))))

;;;--------------------------------------------------------------------
;;; User Creation
;;;--------------------------------------------------------------------
(defun user-create (redis username email password)
 (case (: er setnx redis (key-username-to-uid-ptr username) 'test)
  ('true (let* ((uid (: er incr redis (key-counter-user)))
                (user-key (key-user-hash uid)))
          (: er set redis (key-username-to-uid-ptr username) uid)
          (: er hset redis user-key 'username username)
          (user-email redis uid email)
          (: er hset redis user-key 'ts-signup (now-s))
          (password-set redis user-key password)
          uid))
  ('false 'user_exists)))

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
 (: er hset redis (key-user-hash uid) 'email email))

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

(defun username-to-uid (redis username)
 (: er get redis (key-username-to-uid-ptr username)))

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

(defun user-feature-exists (redis uid feature)
 (: er hexists redis (key-user-feature-hash uid) feature))

(defun user-features (redis uid)
 (: er hkeys redis (key-user-feature-hash uid)))

;;;--------------------------------------------------------------------
;;; Subscription Updating
;;;--------------------------------------------------------------------
(defun user-subscribe (redis uid plan expires)
 (user-feature-set redis uid 'subscription plan)
 (user-subscribe-expiration redis uid expires))

(defun user-subscribe-expiration (redis uid expires)
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
 (user-feature-set redis uid 'payment-uid (: eru er_key provider provider-id))
 (: er set (key-payment-provider-uid-map provider provider-id) uid))

(defun payment-record (redis uid payment-provider invoice-id)
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
