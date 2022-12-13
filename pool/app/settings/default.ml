open Entity

type default =
  { default_reminder_lead_time : Value.default_reminder_lead_time
  ; tenant_languages : Value.tenant_languages
  ; tenant_email_suffixes : Value.tenant_email_suffixes
  ; tenant_contact_email : Value.tenant_contact_email
  ; inactive_user_disable_after : Value.inactive_user_disable_after
  ; inactive_user_warning : Value.inactive_user_warning
  ; trigger_profile_update_after : TriggerProfileUpdateAfter.t
  ; terms_and_conditions : Value.terms_and_conditions
  }
[@@deriving eq, show]

let get_or_failwith = Pool_common.Utils.get_or_failwith
let tenant_languages = Pool_common.Language.[ En; De ]

let terms_en =
  {|Studies in the computer laboratory
Some studies take place in a computer laboratory where each participant sits at a computer and makes decisions based on detailed instructions. Your decisions have an immediate impact on your own earnings and the earnings of the other participants. Experience has shown that students enjoy participating in such studies. In order to participate you must be at least 18 years old.

Rules for studies in the computer laboratory
- A specific number of registered subjects are invited to every study by email. Only invited subjects are allowed to participate in a given study.
- Participants can enroll for a specific time slot via a link sent to them by email. This enrollment is binding.
- All participants are paid at the end of the study. Earnings are paid in Swiss Francs. The amount of money you receive is dependent on participants’ own decisions as well as on those of the other participants.
- Certificates of participation are not signed.
- Participants who arrive late or do not show up at all should note that their failure to attend may result in the study’s cancellation, as studies can only be carried out if there are sufficient participants. The cancellation of a study may cause costs of several thousand Swiss Francs. As a consequence, we are forced to exclude from future studies those persons who fail to show up without a good reason.
- Neuroeconomic studies
- In the field of neuroeconomics, researchers try find out what happens in the brain during the process of decision-making. In neuroeconomic studies, different methods, such as imaging procedures and pharmacology, may be applied.

Rules for neuroeconomic studies
- Participants are contacted by telephone to make an appointment. Participants are informed about procedures as well aspayment details, and are given the opportunity to ask questions. Based on the information received, they can decide whether or not they want to participate in that particular study. At this point, they can decline the offer without any consequences.
- Once a participant has enrolled for a specific time slot, that enrollment is binding. The same rules as for studies in the computer laboratory apply.

Database of participants
- The data stored in the recruitment system are used for scientific studies only. These data will not be passed on to third parties. The data will be used for the following purposes:
  - to inform participants about the latest studies;
  - to invite participants to the latest studies;
  - to check the presence/absence of enrolled participants;
  - to ascertain their participation in previous studies.
- Participants are able to determine whether or not they receive invitations to the latest studies.
- Participation is limited.

Studies
- In the course of the studies, data are generated from the participants’ decisions.
- These data are analyzed by the respective institute.
- The data generated from participants’ decisions are made anonymous and cannot be assigned to any specific participant. Consequently, participation is anonymous.
- The data generated and made anonymous are used in scientific papers and lectures. These papers and lectures are published.
|}
;;

let terms_de =
  {|Studien im Computerlabor
Studien im Computerlabor finden in einem Labor mit mehreren Computerarbeitsplätzen statt. Alle Teilnehmerinnen und Teilnehmer sitzen an einem PC und müssen aufgrund detaillierter Erklärungen Entscheidungen fällen. Diese Entscheidungen haben direkten Einfluss auf Ihre Auszahlung und die Auszahlung der anderen Teilnehmenden. Die Erfahrung zeigt, dass die meisten Studierenden gerne an diesen Studien teilnehmen. Für alle Studien gilt ein Mindestalter von 18 Jahren.

Regeln für Studien im Computerlabor
- Für jede Studie erhält eine bestimmte Zahl registrierter Personen eine Einladung per E-Mail. Nur eingeladene Personen sind zur Teilnahme an der Studie berechtigt.
- Über den in der Email enthaltenen Link können Sie sich direkt im System für einen konkreten Termin registrieren. Diese Anmeldung ist eine verbindliche Zusage, an dieser Studie teilzunehmen.
- Alle Teilnehmenden erhalten am Ende der Studie eine Entschädigung. Diese wird in bar in Schweizerfranken ausbezahlt. Die Höhe des ausbezahlten Betrages hängt von den eigenen Entscheidungen, sowie von den Entscheidungen der anderen Studienteilnehmenden ab.
- Versuchspersonenscheine werden nicht unterschrieben.
- Teilnehmende, die sich für eine bestimmte Studie angemeldet haben und nicht bzw. zu spät erscheinen, müssen sich der Tatsache bewusst sein, dass sie damit die Durchführung der Studie wegen Mangel an Teilnehmenden gefährden. Die Absage einer Studie kann uns Kosten in der Höhe von mehreren tausend Franken verursachen. Aus diesem Grund sehen wir uns leider gezwungen, Personen, die unentschuldigt nicht erscheinen, von sämtlichen weiteren Studien auszuschliessen.

Neuroökonomie-Studien
In der Neuroökonomie wird versucht herauszufinden, was im Hirn während Entscheidungssituationen passiert. In diesen Studien kommen neben den wirtschaftswissenschaftlichen Experimenten beispielsweise auch bildgebende Verfahren und pharmakologische Methoden zum Einsatz.

Regeln für Neuroökonomie-Studien
- Für neuroökonomische Studien werden wir mit Ihnen telefonisch Kontakt aufnehmen, um Fragen zu klären und Termine zu vereinbaren.
- Am Telefon wird Ihnen der genaue Ablauf, die verwendeten Methoden, sowie die Auszahlung im Detail erklärt und Sie haben die Möglichkeit, Fragen zu stellen. Erst dann müssen Sie sich entscheiden, ob Sie Interesse haben, an dieser konkreten Studie teilzunehmen. Sie haben also immer die Möglichkeit, nicht teilzunehmen, ohne dass dies Konsequenzen auf weitere Anfragen hätte.
- Sobald Sie für einen konkreten Termin zugesagt haben, ist Ihre Anmeldung verbindlich und es gelten dieselben Regeln wie bei den Labor-Studien.

Teilnehmerdatenbank
- Die im Online Rekrutierungssystem für wissenschaftliche Studien zum Entscheidungsverhalten enthaltene Daten dienen ausschliesslich der Organisation von wissenschaftlichen Studien. Diese Daten werden nicht an Dritte weitergegeben. Wir benutzen die Daten zu den folgenden Zwecken:
  - um die Teilnehmenden über neue Studien zu informieren
  - um die Teilnehmenden zu neuen Studien einzuladen
  - um das Erscheinen bzw. Nicht-Erscheinen der angemeldeten Teilnehmenden bei Studien zu überprüfen
  - um die Teilnahme an früheren Studien festzustellen
- Jeder Teilnehmende kann jederzeit bestimmen, dass er keine weiteren Einladungen zu Studien erhalten will.
- Die Teilnahmen sind limitiert

Studien
- Bei der Durchführung von Studien werden durch die dabei zu treffenden Entscheidungen der Teilnehmenden Daten generiert.
- Diese Daten werden wissenschaftlich durch die Forschenden des jeweiligen Institutes ausgewertet.
- Die Entscheidungsdaten werden anonymisiert und können keiner Person zugeordnet werden. Die Teilnahme an den Studien ist in diesem Sinne anonym.
- Die generierten, anonymisierten Daten werden für die Erstellung von wissenschaftlichen Forschungsarbeiten und Vorträgen benutzt. Diese Arbeiten werden veröffentlicht.
|}
;;

let default_reminder_lead_time =
  14400
  |> Ptime.Span.of_int_s
  |> Pool_common.Reminder.LeadTime.create
  |> get_or_failwith
;;

let tenant_email_suffixes =
  CCList.map
    (fun m -> m |> EmailSuffix.create |> get_or_failwith)
    [ "econ.uzh.ch"; "uzh.ch" ]
;;

let tenant_contact_email =
  ContactEmail.create "pool@econ.uzh.ch" |> get_or_failwith
;;

let inactive_user_disable_after =
  InactiveUser.DisableAfter.create "5" |> get_or_failwith
;;

let inactive_user_warning = InactiveUser.Warning.create "7" |> get_or_failwith

let trigger_profile_update_after =
  InactiveUser.Warning.create "365" |> get_or_failwith
;;

let terms_and_conditions =
  [ "EN", terms_en; "DE", terms_de ]
  |> CCList.map (fun (language, text) ->
       TermsAndConditions.create language text |> get_or_failwith)
;;

let default_values =
  { default_reminder_lead_time
  ; tenant_languages
  ; tenant_email_suffixes
  ; tenant_contact_email
  ; inactive_user_disable_after
  ; inactive_user_warning
  ; trigger_profile_update_after
  ; terms_and_conditions
  }
;;
