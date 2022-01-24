(*------------------------------------------------------------------------------
LetoScript.inc

Compilation directives for LetoScript (Moneo).

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

{ LetoScript's database options:
  To enable SQL support in LetoScript, first uncomment the LETOSCRIPT_SQL
  definition, then uncomment ONE of the following defines, to indicate
  which library is being used for the SQL support. Leave everything commented
  for no SQL support; LetoScript will still compile, and will be notably
  smaller (but not necessarily faster).

  LETOSCRIPT_SQL_ZEOS (Win32: beta, Linux: not tested)
    Uses ZeosLib for SQL access. ZeosLib is a high-powered cross-platform
    DBO for many SQL protocalls. However, it requires the DB lib(s), which
    requires Delphi / Kylix Pro, and has additional DLL / SO requirements.

  LETOSCRIPT_SQL_LETO (Win32: beta, Linux: not tested)
    This is a lightweight library I wrote myself, for LetoScript specifically.
    Its primary purpose is SQLite support (3.x, where ZeosLib only supports
    2.8 at the time of this writing). It is also cross-platform, but
    requires no extra DLL / SO, nor the DB libs. MySQL support and ODBC
    may be added later, but the intent is for this lib to remain small,
    easy to maintain and easy to compile, and intended for end-users who
    don't want a hefty and/or technical database setup.

}
//{$DEFINE LETOSCRIPT_SQL}
{$IFDEF LETOSCRIPT_SQL}
  //{$DEFINE LETOSCRIPT_SQL_ZEOS}
  //{$DEFINE LETOSCRIPT_SQL_LETO}
{$ENDIF}

{ LetoScript and SCORCO hooking:
  Currently experimental.
}

//{$DEFINE LETOSCRIPT_SCORCO}

