module EmailConfirmation = struct
  type t =
    | Title
    | Note
end

module SignUp = struct
  type t =
    | AcceptTermsAndConditions
    | Title
end

type t =
  | EmailConfirmation of EmailConfirmation.t
  | SignUp of SignUp.t

module De = struct
  module EmailConfirmation = struct
    include EmailConfirmation

    let to_string = function
      | Title -> "Bestätigung Ihrer Email Adresse"
      | Note ->
        "Bitte prüfen Sie zunächst Ihre E-Mails und bestätigen Sie Ihre \
         Adresse."
    ;;
  end

  module SignUp = struct
    include SignUp

    let to_string = function
      | AcceptTermsAndConditions -> "Ich akzeptiere die Nutzungsbedingungen."
      | Title -> "Registrieren"
    ;;
  end

  let to_string m =
    match m with
    | EmailConfirmation m -> EmailConfirmation.to_string m
    | SignUp m -> SignUp.to_string m
  ;;
end

module En = struct
  module EmailConfirmation = struct
    include EmailConfirmation

    let to_string = function
      | Title -> "Email confirmation"
      | Note -> "Please check your emails and confirm your address first."
    ;;
  end

  module SignUp = struct
    include SignUp

    let to_string = function
      | AcceptTermsAndConditions -> "I accept the terms and conditions."
      | Title -> "Sign up"
    ;;
  end

  let to_string m =
    match m with
    | EmailConfirmation m -> EmailConfirmation.to_string m
    | SignUp m -> SignUp.to_string m
  ;;
end
