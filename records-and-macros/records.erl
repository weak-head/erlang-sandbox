-module(records).

-export([verify_signature/1, verify_identity/1]).

-record(credentials, {login, password, identity=none, digital_signature=none}).
-record(identity, {fingerpint=none, footprint=none}).

verify_signature(#credentials{digital_signature=Signature} = _Credentials) ->
    case Signature of
        none -> untrusted;
        _    -> ok
    end.

verify_identity(#credentials{identity=#identity{fingerpint=Fingerpint, footprint=Footprint}}) ->
    if
        (Fingerpint == none) and (Footprint == none) ->
            untrusted;

        true ->
            ok
    end;
verify_identity(_) -> untrusted.
