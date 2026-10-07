(** The failure every database call raises, carrying the label it happened on and
    the untouched caqti error.

    The old path — [Pools.raise_caqti_error] — raised [Caqti_error.Exn], so code
    that needed to tell "already exists" from anything else had to match on
    rendered error strings, and the label was lost. {!cause} exposes caqti's own
    classification instead.

    Deliberately pure — no Lwt, no pool access — so both {!Pools_new} and
    {!Database_new} can depend on it. *)

type failure =
  { label : Entity.Label.t
  ; error : Caqti_error.t
  }

(** Raised by everything that talks to the database. Match on {!cause} — mapping
    [`Unique_violation] onto an "already exists" message, for instance — instead
    of on rendered error strings. *)
exception Failed of failure

val create : Entity.Label.t -> Caqti_error.t -> failure

(** caqti's classification of a statement the server rejected. [None] for the
    errors that carry no cause: connecting, encoding, decoding.

    Note that caqti-driver-mariadb 2.3.x reports deadlocks and lock wait timeouts
    as [`Unspecified__don't_match]; telling those apart would mean reading the
    errno out of the rendered message, which is not worth it until we see one. *)
val cause : Caqti_error.t -> Caqti_error.cause option

(** {!cause} for an exception, covering both {!Failed} and [Caqti_error.Exn] —
    the latter is what the legacy [Pools] entry points raise, so one handler works
    while both are in use. *)
val cause_of_exn : exn -> Caqti_error.cause option
