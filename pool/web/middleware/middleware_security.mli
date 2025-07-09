(** {1 Security Middleware for OCaml/Pool Application}

    A comprehensive security middleware that detects and blocks malicious requests
    based on real attack patterns observed in production access logs.

    {2 Attack Patterns Detected}

    {3 SQL Injection Attacks}
    - Time-based injections: [pg_sleep(0)], [waitfor delay '0:0:3']
    - Boolean-based injections: [OR 2+665-665-1=0+0+0+1], [AND 2*3*8=6*8]
    - MySQL-specific: [XOR(if(now()=sysdate(),sleep(15),0))]
    - Encoded payloads: [%20OR%20], [%27%20AND%20]

    {3 URL Manipulation Attacks}
    - External redirects: [%22https:/www.uzh.ch/de/privacy.html]
    - Suspicious paths: [/cn2qm6w23egz.html]
    - URL encoding attacks: Various [%22], [%27], [%25] patterns

    {3 Additional Protection}
    - XSS attempts ([<script>], [javascript:], [onerror=])
    - Directory traversal ([../], [..\\])
    - Command injection ([;], [&&], [||], [|])
    - LDAP injection ([(], [)], [*])
    - File inclusion attempts ([file://], [ftp://])

    {2 Key Features}

    - {b URL Decoding}: Handles single and double-encoded attacks
    - {b Pattern Matching}: Extensive pattern library based on real attack examples
    - {b Request Analysis}: Checks URLs, query parameters, and headers
    - {b Body Scanning}: Enhanced version also scans POST/PUT request bodies
    - {b Logging}: Detailed logging of blocked requests with IP, User-Agent, etc.
    - {b Configurable Response}: Returns 400 Bad Request for blocked attempts

    {2 Integration Example}

    {[
      let global_middlewares =
        [ CustomMiddleware.Security.middleware ()  (* Security middleware *)
        ; CustomMiddleware.Error.middleware ()
        ; CustomMiddleware.TrailingSlash.middleware ()]
    ]}
*)

(** Check if a request contains suspicious patterns.

    @param request The HTTP request to analyze
    @return [Some categories] if suspicious patterns found, [None] if clean

    Analyzes the request URL, query parameters, and headers for malicious patterns.
    Returns a list of attack categories detected (e.g., ["sql_injection"; "xss"]).

    Example:
    {[
      let request = ... in
      match check_request_security request with
      | Some ["sql_injection"] -> (* Block SQL injection attempt *)
      | Some categories -> (* Block multi-category attack *)
      | None -> (* Allow legitimate request *)
    ]} *)
val check_request_security : Sihl.Web.Request.t -> string list option

(** Create basic security middleware that checks URLs, query params, and headers.

    @return Rock middleware that automatically screens incoming requests

    This middleware:
    - Analyzes request paths, query parameters, and headers
    - Blocks requests containing malicious patterns
    - Returns HTTP 400 for blocked requests
    - Logs suspicious activity
    - Allows clean requests to proceed

    Example usage:
    {[
      let middlewares = [ Security.middleware () (* other middlewares *) ]
    ]} *)
val middleware : unit -> Rock.Middleware.t

(** Create enhanced security middleware that also checks POST/PUT bodies.

    @return Rock middleware with body content scanning

    Like [middleware] but additionally:
    - Scans POST and PUT request bodies for malicious content
    - Provides deeper protection against form-based attacks
    - May have slight performance impact due to body parsing

    Recommended for applications handling user-generated content. *)
val enhanced_middleware : unit -> Rock.Middleware.t

(** Log suspicious request for monitoring and analysis.

    @param request The suspicious HTTP request
    @param categories List of attack categories detected

    Logs detailed information including:
    - Request path and method
    - Client IP address
    - User-Agent string
    - Attack categories detected
    - Timestamp *)
val log_suspicious_request : Sihl.Web.Request.t -> string list -> unit
