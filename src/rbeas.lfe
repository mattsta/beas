(defmodule rbeas
 (export all))

(defmacro zs-call
 ([fn redis-name args]
  `(defun ,fn ,args
    (: beas ,fn ',redis-name ,@args))))

(defmacro mk-beas-tied-to-redis-name
 ([redis-name]
  `(progn
    (zs-call nouser-create ,redis-name (username))
    (zs-call user-create ,redis-name (username email password))
    (zs-call password-set ,redis-name (key password))
    (zs-call user-email ,redis-name (uid email))
    (zs-call user-disable ,redis-name (uid))
    (zs-call user-enable ,redis-name (uid))
    (zs-call user-feature-set ,redis-name (uid feature value))
    (zs-call user-feature-del ,redis-name (uid feature))
    (zs-call user-feature-incr ,redis-name (uid feature delta))
    (zs-call username-exists ,redis-name (username))
    (zs-call email-exists ,redis-name (email))
    (zs-call username-to-uid ,redis-name (username))
    (zs-call email-to-uid ,redis-name (email))
    (zs-call user-username ,redis-name (uid))
    (zs-call user-email ,redis-name (uid))
    (zs-call user-password-match ,redis-name (uid attempted-password))
    (zs-call user-signup-ts ,redis-name (uid))
    (zs-call user-is-disabled ,redis-name (uid))
    (zs-call user-feature-value ,redis-name (uid feature))
    (zs-call user-feature-get ,redis-name (uid feature))
    (zs-call user-feature ,redis-name (uid feature))
    (zs-call user-feature-exists ,redis-name (uid feature))
    (zs-call user-features ,redis-name (uid))
    (zs-call user-subscribe ,redis-name (uid plan expires))
    (zs-call user-subscribe-expire ,redis-name (uid expires))
    (zs-call user-unsubscribe ,redis-name (uid))
    (zs-call user-subscription ,redis-name (uid))
    (zs-call user-subscription-expires ,redis-name (uid))
    (zs-call user-location-marker-new ,redis-name
     (uid name lat lng start stop recur))
    (zs-call gen-marker-id ,redis-name ())
    (zs-call user-location-marker-disable ,redis-name (uid marker-id))
    (zs-call user-location-marker-is-disabled ,redis-name (uid marker-id))
    (zs-call user-location-marker-enable ,redis-name (uid marker-id))
    (zs-call user-locations ,redis-name (uid))
    (zs-call payment-uid-map-set ,redis-name (provider provider-id uid))
    (zs-call payment-record ,redis-name (uid payment-provider invoice-id))
    (zs-call payments ,redis-name (uid)))))

(mk-beas-tied-to-redis-name redis_beas)
