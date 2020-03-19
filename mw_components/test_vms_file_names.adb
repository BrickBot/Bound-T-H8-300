with TEXT_IO,
     VARYING_TEXT,
     VARYING_TEXT_IO,
     VMS_FILE_NAMES,
     VMS_LOGICAL_NAMES;

use TEXT_IO,
    VARYING_TEXT,
    VARYING_TEXT_IO,
    VMS_FILE_NAMES,
    VMS_LOGICAL_NAMES;

procedure TEST_VMS_FILE_NAMES is
-----------------------------

  FILE_NAME,
  DEFAULT,
  LOGICAL_NAME,
  EQUIVALENCE_NAME   : TEXT(255);

  procedure PUT_ALL_MATCHING_NAMES is new ENUMERATE_MATCHING_FILES(PUT_LINE);

begin
  PUT_LINE("Assignment :");
  loop
    PUT("   Logical Name : ");
    GET_LINE(LOGICAL_NAME);
    exit when EQUAL(LOGICAL_NAME, "");
    PUT("   Equivalence Name : ");
    GET_LINE(EQUIVALENCE_NAME);
    DEFINE(TO_STRING(LOGICAL_NAME), TO_STRING(EQUIVALENCE_NAME));
    NEW_LINE;
  end loop;
  PUT_LINE("Translation :");
  loop
    PUT("   Logical Name : ");
    GET_LINE(LOGICAL_NAME);
    exit when EQUAL(LOGICAL_NAME, "");
    PUT("   Equivalence Name : ");
    PUT(TRANSLATE(TO_STRING(LOGICAL_NAME)));
    NEW_LINE;
  end loop;
  loop
    PUT("File Name : ");
    GET_LINE(FILE_NAME);
    PUT("Default : ");
    GET_LINE(DEFAULT);
    exit when EQUAL(FILE_NAME, "") and EQUAL(DEFAULT, "");
    PUT_LINE('"' & FULL_FILE_NAME(TO_STRING(FILE_NAME),
                                  TO_STRING(DEFAULT)) & '"');
    NEW_LINE;
    declare

      FILE_FIELDS : constant FILE_NAME_FIELDS :=
        FULL_FILE_NAME(TO_STRING(FILE_NAME), TO_STRING(DEFAULT));

    begin
      PUT_LINE(FILE_FIELDS.NODE_NAME);
      PUT_LINE(FILE_FIELDS.DEVICE);
      PUT_LINE(FILE_FIELDS.DIRECTORY);
      PUT_LINE(FILE_FIELDS.FILE_NAME);
      PUT_LINE(FILE_FIELDS.FILE_TYPE);
      PUT_LINE(FILE_FIELDS.VERSION);
      NEW_LINE;
    end;
    PUT_ALL_MATCHING_NAMES(TO_STRING(FILE_NAME),
                           TO_STRING(DEFAULT));
    NEW_LINE;
  end loop;
  PUT_LINE("Local node name : " & LOCAL_NODE_NAME);
end TEST_VMS_FILE_NAMES;
