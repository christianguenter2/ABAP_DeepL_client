INTERFACE zif_deepl_definitions
  PUBLIC .

  TYPES:
    ty_lang TYPE c LENGTH 2.

  TYPES:
    BEGIN OF ENUM lang BASE TYPE ty_lang,
      en VALUE `EN`,
      de VALUE `DE`,
      fr VALUE `FR`,
      es VALUE `ES`,
      pt VALUE `PT`,
      it VALUE `IT`,
      nl VALUE `NL`,
      pl VALUE `PL`,
      ru VALUE `RU`,
      xx VALUE IS INITIAL,
    END OF ENUM lang.

ENDINTERFACE.
