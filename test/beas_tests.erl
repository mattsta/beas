-module(beas_tests).

-include_lib("eunit/include/eunit.hrl").

%%%----------------------------------------------------------------------
%%% Prelude
%%%----------------------------------------------------------------------
beas_test_() ->
  {setup,
    fun setup/0,
    fun teardown/1,
    [
     {"Create User",
       fun create_user/0},
     {"Deny Create User",
       fun deny_create_user/0},
     {"Login",
       fun login/0},
     {"Assign Service",
       fun assign_service/0},
     {"Assign Features",
       fun assign_features/0},
     {"Check Signup TS",
       fun check_signup_ts/0},
     {"Set and Check Subscription",
       fun set_subscribe/0},
     {"Record Payment from Provider",
       fun subscribe_pay/0},
     {"Set Unsubscribe",
       fun set_unsubscribe/0},
     {"Read Payment Record",
       fun read_payments/0},
     {"Add Location",
       fun new_location/0},
     {"Read Locations",
       fun user_locations/0},
     {"Username Existence",
       fun username_exists/0},
     {"Email Existence",
       fun email_exists/0}
    ]
  }.

%%%----------------------------------------------------------------------
%%% Tests
%%%----------------------------------------------------------------------
create_user() ->
  Uid = beas:'user-create'(tester, matt, "matt@matt.matt", "mattpass"),
  % username stored: "username:\xc3\xa6\xc2\x97\xc2\xa5\xc3\xa6\xc2\x9c\xc2\xac"
  Uid2 = beas:'user-create'(tester, "日本", "matt@matt2.matt", "mattpass"),
  ?assertEqual(1, Uid),
  ?assertEqual(2, Uid2),
  Email = beas:'user-email'(tester, 1),
  ?assertEqual(<<"matt@matt.matt">>, Email),
  UidA = beas:'email-to-uid'(tester, "matt@matt.matt"),
  ?assertEqual(<<"1">>, UidA),
  UidB = beas:'email-to-uid'(tester, "matt@matt2.matt"),
  ?assertEqual(<<"2">>, UidB),
  % test user exists and email exists errors
  UidNO_U = beas:'user-create'(tester, "日本", "matt@matt2.matt", "mattpass"),
  UidNO_E = beas:'user-create'(tester, "日本p", "matt@matt2.matt", "mattpass"),
  ?assertEqual(user_exists, UidNO_U),
  ?assertEqual(email_exists, UidNO_E),
  % test email exists error when changing email ddress
  NewEmail = beas:'user-email'(tester, Uid, "matt@matt2.matt"),
  ?assertEqual(email_exists, NewEmail),
  % test chaning email address actually works
  NewEmail2 = beas:'user-email'(tester, Uid, "matt@matt2.matt-reallynew.com"),
  ?assertEqual(false, NewEmail2). % "false" means "field was updated properly."


deny_create_user() ->
  Error = beas:'user-create'(tester, matt, nil, nil),
  ?assertEqual(user_exists, Error).

login() ->
  LoginWorked = beas:'user-password-match'(tester, 1, "mattpass"),
  LoginFailed = beas:'user-password-match'(tester, 1, "poopin"),
  ?assertEqual(true, LoginWorked),
  ?assertEqual(false, LoginFailed).

assign_service() ->
  beas:'user-subscribe'(tester, 1, bling, never).
 
assign_features() ->
  beas:'user-feature-set'(tester, 1, sixmonthforum, yes),
  ForumValue = beas:'user-feature-value'(tester, 1, sixmonthforum),
  ?assertEqual(<<"yes">>, ForumValue).

check_signup_ts() ->
  Z = beas:'user-signup-ts'(tester, 1),
  ?assertEqual(true, is_integer(Z)).

set_subscribe() ->
  beas:'user-subscribe'(tester, 1, bling, blong),
  Sub = beas:'user-subscription'(tester, 1),
  ?assertEqual(<<"bling">>, Sub).

set_unsubscribe() ->
  beas:'user-unsubscribe'(tester, 1),
  Sub = beas:'user-subscription'(tester, 1),
  ?assertEqual(nil, Sub).

subscribe_pay() ->
  beas:'payment-record'(tester, 1, stripe, bob).

read_payments() ->
  PayLog = beas:'payments'(tester, 1),
  ?assertEqual([<<"stripe:bob">>], PayLog).

new_location() ->
  Id = beas:'user-location-marker-new'(tester, 1, bob, 3, 5, 300, 600, daily),
  ?assertEqual(1, Id).

user_locations() ->
  Locations = beas:'user-locations'(tester, 1),
  ?assertEqual([[{uid, <<"1">>},
                 {name, <<"bob">>},
                 {lat, <<"3">>},
                 {lng, <<"5">>},
                 {start, <<"300">>},
                 {stop, <<"600">>},
                 {recur, <<"daily">>}]], Locations).

username_exists() ->
  Yes = beas:'username-exists'(tester, matt),
   No = beas:'username-exists'(tester, poopin),
  ?assertEqual(true, Yes),
  ?assertEqual(false, No).

email_exists() ->
  Yes = beas:'email-exists'(tester, "matt@matt2.matt-reallynew.com"),
   No = beas:'email-exists'(tester, zOOOOOOOOOOOOO),
  ?assertEqual(true, Yes),
  ?assertEqual(false, No),
  % This is the address existing before an email update.  make sure it doesn't
  % exist any longer:
  Nope = beas:'email-exists'(tester, "matt@matt.matt"),
  ?assertEqual(false, Nope).

%%%----------------------------------------------------------------------
%%% Setup / Cleanup
%%%----------------------------------------------------------------------
setup() ->
  application:start(er),
  er_pool:start_link(tester, "127.0.0.1", 9961),
  er:flushall(tester). % we need an empty redis because we count IDs in tests

teardown(_) ->
  ok.
