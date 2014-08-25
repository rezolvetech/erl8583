{application, erl8583,
    [
        {description, "ISO 8583 library"},
        {vsn, "0.3.6"},
        {modules,   [
                    erl8583_convert,
                    erl8583_fields,
                    erl8583_fields_1993,
                    erl8583_fields_2003,
                    erl8583_marshaller,
                    erl8583_marshaller_ascii,
                    erl8583_marshaller_binary,
                    erl8583_marshaller_ebcdic,
                    erl8583_marshaller_json,
                    erl8583_marshaller_xml,
                    erl8583_message
                    ]},
        {registered, []},
        {env, []},
        {applications, [kernel, stdlib]}
    ]
}.
